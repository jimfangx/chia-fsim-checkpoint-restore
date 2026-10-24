// See LICENSE for license details.

package midas.stage

import midas.{HostTransforms, TargetTransforms}
import midas.passes.MidasTransforms
import midas.stage.phases.{ConfigParametersAnnotation, CreateParametersInstancePhase}

import firrtl.{AnnotationSeq, CircuitState}
import firrtl.options.{Dependency, Phase}
import firrtl.passes.memlib.InferReadWrite
import firrtl.stage.{FirrtlCircuitAnnotation, Forms}
import firrtl.stage.transforms.Compiler

/** Immutable identity of the stage input already consumed by Golden Gate.
  * Keeping FirrtlCircuitAnnotation here leaves a second, stale circuit in the
  * transformed simulator. This receipt is history, never a replacement input.
  */
case class GoldenGateInputCircuitAnnotation(
  circuit: String,
  circuitSHA256: String,
  modules: Seq[String],
) extends firrtl.annotations.NoTargetAnnotation

object GoldenGateCompilerPhase {
  private[midas] def consumeInputCircuit(annotations: AnnotationSeq): (firrtl.ir.Circuit, AnnotationSeq) = {
    val circuits = annotations.collect { case FirrtlCircuitAnnotation(c) => c }
    require(circuits.size == 1, "Golden Gate can only process a single Firrtl Circuit at a time.")
    require(!annotations.exists(_.isInstanceOf[GoldenGateInputCircuitAnnotation]),
      "Golden Gate input circuit has already been consumed")
    val circuit = circuits.head
    val history = GoldenGateInputCircuitAnnotation(circuit.main,
      midas.checkpoint.SemanticStateInventory.sha256(circuit.serialize), circuit.modules.map(_.name))
    (circuit, annotations.map {
      case _: FirrtlCircuitAnnotation => history
      case a => a
    })
  }
}

class GoldenGateCompilerPhase extends Phase {

  override val prerequisites = Seq(
    Dependency(midas.stage.Checks),
    Dependency(midas.stage.AddDerivedAnnotations),
    Dependency[CreateParametersInstancePhase],
  )

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val (inputCircuit, consumedAnnotations) = GoldenGateCompilerPhase.consumeInputCircuit(annotations)
    midas.checkpoint.SimulatorFPGASynthesisConstants.validateCardinality(annotations)

    implicit val p = annotations.collectFirst({ case ConfigParametersAnnotation(p) => p }).get

    val midasAnnos = Seq(
      firrtl.passes.memlib.InferReadWriteAnnotation,
      firrtl.passes.memlib.DefaultReadFirstAnnotation,
      firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
    )

    val state = CircuitState(inputCircuit, firrtl.ChirrtlForm, consumedAnnotations ++ midasAnnos)

    // Lower the target design and run additional target transformations before Golden Gate xforms
    val targetLoweringCompiler = new Compiler(
      Seq(
        Dependency[midas.passes.RunConvertAssertsEarly],
        Dependency(firrtl.transforms.formal.ConvertAsserts),
        Dependency[firrtl.passes.memlib.InferReadWrite],
        Dependency[firrtl.transforms.SimplifyMems],
      ) ++
        Forms.LowForm ++
        p(TargetTransforms)
    )
    logger.info("Pre-GG Target Transformation Ordering\n")
    logger.info(targetLoweringCompiler.prettyPrint("  "))
    // The dependency-based Compiler does not update the legacy CircuitState.form field.
    val loweredTarget = midas.checkpoint.SemanticStateInventory(
      targetLoweringCompiler.execute(state).copy(form = firrtl.LowForm)
    )

    // Run Golden Gate transformations, introducing host-decoupling and generating additional simulator RTL
    val simulator =
      new Compiler(Forms.LowForm ++ Seq(Dependency[InferReadWrite], Dependency[MidasTransforms]), Forms.LowForm)
        .execute(loweredTarget)

    compileSimulator(simulator)
  }

  /** Shared production path from the mapped simulator to its final RTL.
    * Keep the returned host-transform state: injected storage, clock wiring and
    * annotations must reach both the FPGA backend and checkpoint inventory.
    */
  private[stage] def compileSimulator(simulator: CircuitState)(
      implicit p: org.chipsalliance.cde.config.Parameters): AnnotationSeq = {
    // Validate cardinality before Compiler canonicalizes annotation sequences.
    midas.checkpoint.SimulatorBankProbe.validateRequest(simulator)
    midas.checkpoint.SimulatorFPGASynthesisConstants.validateRequest(simulator)
    // Note: the final lowering to Verilog is broken up into multiple firrtl.Compiler steps
    // to avoid breakages that can emerge from "legal" but unsound pass orderings
    // due to underspecified pass constraints + invalidations
    // Shunt ILA passes to a seperate compiler to avoid invalidating downstream steps
    val loweringCompiler = new Compiler(
      targets      = Seq(
        Dependency(midas.passes.AutoILATransform),
        Dependency(midas.passes.HostClockWiring),
      ) ++
        Forms.LowForm ++
        p(HostTransforms),
      currentState = Forms.LowForm,
    )
    logger.info("Post-GG Host Transformation Ordering\n")
    logger.info(loweringCompiler.prettyPrint("  "))
    val hostTransformedSimulator = loweringCompiler.execute(simulator)

    // Ensure FPGA backend passes run after all user-provided + ILA transforms, but before final emission
    val fpgaBackendCompiler  = new Compiler(
      targets      = Seq(
        Dependency[firrtl.passes.memlib.SeparateWriteClocks],
        Dependency[firrtl.passes.memlib.SetDefaultReadUnderWrite],
        Dependency[firrtl.transforms.SimplifyMems],
      ) ++
        Forms.LowForm,
      currentState = Forms.LowForm,
    )
    logger.info("FPGA Backend Transformation Ordering\n")
    logger.info(fpgaBackendCompiler.prettyPrint("  "))
    val backendSimulator = fpgaBackendCompiler.execute(hostTransformedSimulator)
    if (backendSimulator.annotations.contains(midas.checkpoint.SimulatorBankProbeAnnotation)) {
      require(p(midas.core.CheckpointCaptureTopKey).isEmpty,
        "Simulator bank probe cannot surround its own live capture transport")
    }
    val fpgaConstants = midas.checkpoint.SimulatorFPGASynthesisConstants(backendSimulator.copy(form = firrtl.LowForm))
    val postLoweredSimulator = midas.checkpoint.SimulatorBankProbe(fpgaConstants)

    // Workaround under-constrained transform dependencies by forcing the
    // emitter to run last in a separate compiler.
    val semanticMap = postLoweredSimulator.annotations.contains(midas.checkpoint.SemanticFAMEStateMapAnnotation)
    val simulatorInventory = postLoweredSimulator.annotations.exists(
      _.isInstanceOf[midas.checkpoint.SimulatorCheckpointInventoryAnnotation])
    // The observer captures the emitter's own VerilogPrep/rename result. Never
    // rerun those transforms on its returned (pre-emission) CircuitState.
    val observeEmission = semanticMap || simulatorInventory
    val verilogEmitter = if (observeEmission) new midas.checkpoint.SemanticCleanEmitter else new firrtl.SystemVerilogEmitter
    val emitter = new Compiler(Seq(Dependency(midas.passes.WriteXDCFile), Dependency.fromTransform(verilogEmitter)),
      Forms.LowForm, Set(verilogEmitter))
    logger.info("Final Emission Transformation Ordering\n")
    logger.info(emitter.prettyPrint("  "))

    val emissionInput = if (semanticMap) midas.checkpoint.SemanticFAMEStateMap.prepareEmission(postLoweredSimulator) else postLoweredSimulator
    val emitted = emitter.execute(if (observeEmission) emissionInput.copy(annotations =
      emissionInput.annotations.filterNot(_.isInstanceOf[firrtl.EmitCircuitAnnotation]) :+
        firrtl.EmitCircuitAnnotation(verilogEmitter.getClass)) else emissionInput)
    val bankProbeFiles = midas.checkpoint.SimulatorBankProbe.emissionBinding(
      postLoweredSimulator, emitted.getEmittedCircuit.value, emitted.annotations)
    val fpgaConstantFiles = midas.checkpoint.SimulatorFPGASynthesisConstants.emissionBinding(
      postLoweredSimulator, emitted.getEmittedCircuit.value)
    val semanticMapFiles = if (!semanticMap) Seq.empty else {
      val actual = verilogEmitter.asInstanceOf[midas.checkpoint.SemanticCleanEmitter].preparedState.get
      Seq(midas.checkpoint.SemanticFAMEStateMap.emit(actual, emitted.getEmittedCircuit.value),
        GoldenGateOutputFileAnnotation(emitted.getEmittedCircuit.value, ".semantic-fame-emitted.sv"))
    }
    if (simulatorInventory) {
      val emissionIR = verilogEmitter.asInstanceOf[midas.checkpoint.SemanticCleanEmitter].preparedState
        .getOrElse(throw new IllegalStateException("checkpoint inventory: emitter did not expose its actual state"))
      val inventoried = midas.checkpoint.SimulatorStateInventory(emissionIR)
      val files = inventoried.annotations.drop(emissionIR.annotations.size)
      val manifest = files.collectFirst {
        case GoldenGateOutputFileAnnotation(body, ".simulator-state.json") => body
      }.get
      val externalOwners = files.collectFirst {
        case GoldenGateOutputFileAnnotation(body, ".simulator-external-owners.json") => body
      }.get
      import org.json4s._
      import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
      val binding = canonical(JObject(List(
        "binding_version" -> JInt(1), "host_pipeline_version" -> JInt(2),
        "scope" -> JString("generated_rtl_only"), "external_runtime_required" -> JBool(true),
        "host_transformed_circuit_sha256" -> JString(sha256(hostTransformedSimulator.circuit.serialize)),
        "fpga_backend_circuit_sha256" -> JString(sha256(postLoweredSimulator.circuit.serialize)),
        "emission_reference_sha256" -> JString(sha256(emissionIR.circuit.serialize)),
        "simulator_manifest_sha256" -> JString(sha256(manifest)),
        "external_owners_sha256" -> JString(sha256(externalOwners)),
        "emitted_verilog_sha256" -> JString(sha256(emitted.getEmittedCircuit.value)))))
      emitted.annotations ++ files ++ semanticMapFiles ++ bankProbeFiles ++ fpgaConstantFiles :+
        GoldenGateOutputFileAnnotation(binding, ".simulator-emission.json")
    } else emitted.annotations ++ semanticMapFiles ++ bankProbeFiles ++ fpgaConstantFiles
  }
}
