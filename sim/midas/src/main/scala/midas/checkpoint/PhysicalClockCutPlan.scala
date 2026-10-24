// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance, WDefInstanceConnector, WRef, WSubField}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

case object PhysicalClockCutPlanAnnotation extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[Unit](
    longOption = "simulator-physical-clock-plan",
    toAnnotationSeq = (_: Unit) => Seq(PhysicalClockCutPlanAnnotation),
    helpText = "Audit complete native common-source clock topology; emits a plan, never enables physical bank capture.",
  ))
}

/** Binding prerequisites for the common-low-phase cut, from the final emitted
  * netlist. This is deliberately separate from state ownership admission.
  * In particular a physical CE latch cannot use a metasim O StateID or value.
  */
object PhysicalClockCutPlan {
  import SemanticStateInventory.{canonical, sha256}
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Physical clock cut plan unsupported: $why")

  def validateCardinality(annotations: firrtl.AnnotationSeq): Unit = {
    PhysicalClockCutWiring.validateRequest(annotations)
    val n = annotations.count(_ == PhysicalClockCutPlanAnnotation)
    if (n == 0) return
    if (n != 1) fail("exactly one clock-plan request is required")
    if (annotations.count(_.isInstanceOf[SimulatorCheckpointInventoryAnnotation]) != 1)
      fail("exactly one complete circuit-root inventory is required")
    if (annotations.contains(SimulatorBankProbeAnnotation) || annotations.contains(SemanticFAMEStateMapAnnotation))
      fail("clock planning cannot admit bank capture or semantic mapping")
  }

  def validateRequest(state: CircuitState): Unit = {
    validateCardinality(state.annotations)
    if (state.annotations.contains(PhysicalClockCutPlanAnnotation) &&
        state.annotations.collect { case a: SimulatorCheckpointInventoryAnnotation => a.rootModule } != Seq(state.circuit.main))
      fail("exactly one complete circuit-root inventory is required")
  }

  /** VerilogPrep packs exact port connections into instance connectors. Open
    * them only for analysis, preserving input/output direction and full pin
    * identity. Never infer connectivity from the emitted Verilog spelling.
    */
  private[checkpoint] def explicitConnections(state: CircuitState): CircuitState = {
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    def rewrite(s: Statement): Statement = s match {
      case i: WDefInstanceConnector =>
        val ports = modules.getOrElse(i.module, fail(s"missing connector definition ${i.module}")).ports
        val connections = i.portCons.map { case (pin, value) =>
          val name = pin match {
            case WSubField(WRef(instance, _, _, _), field, _, _) if instance == i.name => field
            case _ => fail(s"nonlocal connector pin at ${i.name}: ${pin.serialize}")
          }
          val p = ports.find(_.name == name).getOrElse(fail(s"unknown connector pin ${i.name}.$name"))
          if (pin.tpe != p.tpe || value.tpe != p.tpe) fail(s"reshaped connector pin ${i.name}.$name")
          name -> (if (p.direction == Input) Connect(NoInfo, pin, value) else Connect(NoInfo, value, pin))
        }
        if (connections.map(_._1).distinct.size != ports.size || connections.size != ports.size)
          fail(s"incomplete or duplicate connector ${i.name}")
        Block(Seq(WDefInstance(i.info, i.name, i.module, i.tpe)) ++ connections.map(_._2))
      case x => x.mapStmt(rewrite)
    }
    state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module => m.copy(body = rewrite(m.body))
      case m => m
    }))
  }

  def discover(state: CircuitState, manifest: String): String = {
    if (state.form != LowForm) fail("resolved LowForm required")
    if (manifest != SimulatorStateInventory.discover(state)) fail("stale or incomplete simulator inventory")
    val analysis = explicitConnections(state)
    val live = GeneratedExternalOwners.instances(analysis, state.circuit.main)
    val native = live.collect { case (path, m: ExtModule) if m.defname == "BUFGCE" =>
      PhysicalClockGateContract.validate(state, m); path -> m
    }.sortBy(_._1)
    if (native.isEmpty) fail("no native default SYNC gates")
    val (domains, consumers) = GeneratedStateBankAccess.physicalClockTopology(analysis)
    def str(row: JValue, key: String): String = (row \ key) match {
      case JString(s) => s
      case _ => fail(s"missing $key in clock origin")
    }
    val byClock = domains.arr.map(r => str(r, "clock_hierarchy") -> r).toMap
    if (byClock.size != domains.arr.size) fail("duplicate clock hierarchy")
    // Metadata clock tokens may have a data/constant/invalid origin. They do
    // not count as oscillators. Every sequential consumer has already passed
    // functional(), and every native gate source is checked separately below.
    val gateSources = native.map { case (path, _) =>
      val origin = byClock.getOrElse(path + ".I", fail(s"unresolved native source $path"))
      if ((origin \ "gate_chain") != JNothing) fail(s"cascaded physical source at $path")
      str(origin, "root_clock")
    }
    val roots = (gateSources ++ consumers.arr.map(r => str(r \ "origin", "root_clock"))).distinct
    if (roots.size != 1) fail(s"common-source cut requires one functional root, found ${roots.sorted.mkString(",")}")
    val gates = native.zipWithIndex.map { case ((path, m), index) =>
      val parent = path.take(path.lastIndexOf('.'))
      val name = path.drop(path.lastIndexOf('.') + 1)
      val body = live.collectFirst { case (`parent`, p: Module) => p }.get
      val drivers = mutable.ArrayBuffer.empty[Expression]
      def visit(s: Statement): Unit = {
        s match { case c: Connect if c.loc.serialize == name + ".CE" => drivers += c.expr; case _ => () }
        s.foreachStmt(visit)
      }
      visit(body.body)
      if (drivers.size != 1 || drivers.head.tpe != UIntType(IntWidth(1))) fail(s"missing/reshaped CE driver at $path")
      val logical = path + ".CE_latch"
      val hardwareID = sha256(canonical(JObject(List("namespace" -> JString("physical-bufgce-ce-latch-v1"),
        "hierarchy" -> JString(logical)))))
      JObject(List("index" -> JInt(index), "hierarchy" -> JString(path),
        "module" -> JString(m.name), "physical_contract" -> PhysicalClockGateContract.binding(state, m),
        "source_clock" -> byClock(path + ".I"), "output_clock" -> byClock(path + ".O"),
        "ce_driver_scope" -> JString(parent), "ce_driver_expression" -> JString(drivers.head.serialize),
        "logical_latch_hierarchy" -> JString(logical), "logical_latch_state_id" -> JString(hardwareID),
        "metasim_output_state_id" -> JString(SimulatorStateInventory.stateID(path + ".O", "register")),
        "capture_mapping" -> JString("pending_common_low_phase_ce_sampler_and_restore_adapter")))
    }
    val consumerPaths = consumers.arr.map(r => str(r, "hierarchy")).toSet
    val stateCoverage = (parse(manifest) \ "state").asInstanceOf[JArray].arr.map { row =>
      val path = str(row, "hierarchy")
      val adapter = str(row, "adapter")
      val owners = adapter match {
        case "metasim-bufgce-v1" => Nil
        case "firrtl-register-v1" => List(path)
        case "firrtl-emitter-memory-pipeline-v1" =>
          val scope = path.take(path.lastIndexOf('.'))
          List(scope + "." + str(row, "clock_expression").stripSuffix(".clk"))
        case "firrtl-memory-v1" => consumerPaths.filter(_.startsWith(path + ".")).toList.sorted
        case _ => fail(s"unmapped state clock adapter $adapter at $path")
      }
      if (!owners.forall(consumerPaths)) fail(s"unmapped sequential state at $path")
      JObject(List("state_id" -> (row \ "state_id"), "hierarchy" -> JString(path),
        "clock_consumers" -> JArray(owners.map(JString(_))),
        "physical_latch_mapping_required" -> JBool(adapter == "metasim-bufgce-v1")))
    }
    canonical(JObject(List("plan_version" -> JInt(1), "scope" -> JString("physical_clock_topology_only"),
      "root_module" -> JString(state.circuit.main), "common_source_clock" -> JString(roots.head),
      "reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "simulator_manifest_sha256" -> JString(sha256(manifest)),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "clock_rewrite_applied" -> JBool(false), "ce_dependency_closure_proven" -> JBool(false),
      "physical_gates" -> JArray(gates.toList), "clock_domains" -> domains,
      "sequential_consumers" -> consumers,
      "state_clock_coverage" -> JArray(stateCoverage),
      "remaining_obligations" -> JArray(Seq("all_functional_and_ce_driver_owners_use_generated_gated_clocks",
        "external_ce_and_reset_writers_are_frozen_at_the_same_cut", "physical_latch_bank_and_metasim_restore_mapping",
        "functional_memory_timing_bridge_host_and_pre_fame_ownership", "connected_ungated_mmio_dma_transport")
        .map(JString(_)).toList))))
  }

  def emit(state: CircuitState, manifest: String, verilog: String): Seq[GoldenGateOutputFileAnnotation] = {
    validateRequest(state)
    if (!state.annotations.contains(PhysicalClockCutPlanAnnotation)) return Nil
    if (state.annotations.exists {
      case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(".physical-clock-plan")
      case _ => false
    }) fail("existing physical clock plan output")
    val plan = discover(state, manifest)
    Seq(GoldenGateOutputFileAnnotation(plan, ".physical-clock-plan.json"),
      GoldenGateOutputFileAnnotation(canonical(JObject(List("binding_version" -> JInt(1),
        "scope" -> JString("physical_clock_topology_only"), "complete_fpga_capture" -> JBool(false),
        "deployable_capture_image" -> JBool(false), "reference_sha256" -> JString(sha256(state.circuit.serialize)),
        "simulator_manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
        "emitted_verilog_sha256" -> JString(sha256(verilog))))), ".physical-clock-plan-emission.json")) ++
      PhysicalClockCutWiring.emit(state, manifest, plan)
  }
}
