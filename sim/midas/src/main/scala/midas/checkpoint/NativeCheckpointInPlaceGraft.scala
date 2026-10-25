// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import midas.targetutils.xdc.XDCPathToCircuitAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Post-lowering transaction/CE router inside the existing execution root.
  * Unlike the elaboration companion, this never instantiates an enclosing
  * simulator. Original hierarchy, clocks and constraint paths stay in place.
  * Capture ports remain explicit: the production widget/owner connection and
  * complete simulator restore admission are separate, mandatory contracts.
  */
object NativeCheckpointInPlaceGraft {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, files: Seq[GoldenGateOutputFileAnnotation], binding: String)

  def capturePorts(gates: Int): Seq[Port] = {
    require(gates > 0 && gates <= 65535, "native in-place graft: invalid physical gate count")
    def p(name: String, width: Int, direction: Direction) =
      Port(NoInfo, "capture_" + name, direction, UIntType(IntWidth(width)))
    Seq(p("reset", 1, Input), p("clock_mask", 1, Input), p("read_enable", 1, Input),
      p("captured_ce", gates, Input), p("logical_ce", gates, Output),
      p("descriptor_entry", 32, Input), p("descriptor_valid", 1, Output),
      p("descriptor_bank", 8, Output), p("descriptor_slot", 16, Output),
      p("descriptor_width", 16, Output), p("descriptor_depth", 32, Output),
      p("request_valid", 1, Input), p("request_ready", 1, Output),
      p("bank", 8, Input), p("slot", 16, Input), p("address", 32, Input),
      p("response_valid", 1, Output), p("response_ready", 1, Input),
      p("response_data", 64, Output), p("response_error", 1, Output))
  }

  def generate(result: NativeCheckpointInPlaceContext.Result,
      placement: XDCPathToCircuitAnnotation = XDCPathToCircuitAnnotation(None, None)): Result = {
    // This independently reconstructs the original host/context/identity before
    // consuming any supplied sidecar or generated circuit.
    val checked = NativeCheckpointInPlaceAccess.emit(result)
    val descriptors = NativeCheckpointInPlaceAccess.descriptors(result)
    val state = result.state
    require(state.form == LowForm, "native in-place graft: resolved LowForm required")
    val root = state.circuit.modules.find(_.name == state.circuit.main).get.asInstanceOf[Module]
    val original = result.host.circuit.modules.find(_.name == root.name).get.asInstanceOf[Module]
    val prefix = NativeCheckpointInPlace.prefix
    val access = root.ports.filter(_.name.startsWith(prefix))
    require(access.nonEmpty && root.ports.filterNot(_.name.startsWith(prefix)) == original.ports,
      "native in-place graft: changed original interface")
    val name = root.name + "CheckpointReadoutAdapter"
    val instance = "checkpoint_readout_adapter"
    val declarations = scala.collection.mutable.ArrayBuffer.empty[String]
    def visit(s: Statement): Unit = {
      s match { case d: IsDeclaration => declarations += d.name; case _ => () }
      s.foreachStmt(visit)
    }
    visit(root.body)
    val ports = capturePorts(descriptors.physicalClockGates)
    require(!(declarations ++ root.ports.map(_.name)).exists(n => n == instance || n.startsWith("capture_")) &&
      !state.circuit.modules.exists(_.name == name), "native in-place graft: reserved identifier collision")
    val JString(clock) = parse(result.native.binding) \ "enclosing_raw_clock"
    val clockPort = original.ports.find(p => p.name == clock && p.direction == Input && p.tpe == ClockType)
      .getOrElse(throw new IllegalArgumentException("native in-place graft: missing proven raw clock"))
    val adapterPorts = Seq(clockPort) ++ access.map(p => p.copy(direction =
      if (p.direction == Input) Output else Input))
    val ext = ExtModule(NoInfo, name, adapterPorts ++ ports, name, Nil)
    val connections = ext.ports.map { p =>
      val pin = WSubField(WRef(instance), p.name, p.tpe)
      val wire = WRef(p.name, p.tpe)
      if (p.direction == Input) Connect(NoInfo, pin, wire) else Connect(NoInfo, wire, pin)
    }
    val rewritten = root.copy(ports = original.ports ++ ports,
      body = Block(access.map(p => DefWire(NoInfo, p.name, p.tpe)) ++
        Seq(WDefInstance(NoInfo, instance, name, firrtl.Utils.module_type(ext)), root.body) ++ connections))
    val circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m if m.name == root.name => rewritten
      case m => m
    } :+ ext)
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed)
    firrtl.passes.CheckFlows.run(flowed)
    val ce = PhysicalClockStateBank.emit(result.reference, result.manifest, result.plan, result.dependencies)
    val JString(ceModule) = parse(ce.head.body) \ "endpoint_module"
    val rtl = NativeCheckpointStateAccess.render(original, name, clock, descriptors.physicalClockGates,
      descriptors.entries.filterNot(_.bank == PhysicalClockStateBank.bank), ceModule, prefix, Some(adapterPorts))
    val router = checked.find(_.fileSuffix == ".native-inplace-access-router.sv").get.body
    val grafted = state.copy(circuit = flowed)
    Seq(placement.preLinkPath, placement.postLinkPath).flatten.foreach(p =>
      require(p.split("/", -1).forall(_.matches("[A-Za-z_][A-Za-z0-9_]*")),
        "native in-place graft: placement requires literal hierarchy segments"))
    val storage = grafted.copy(annotations = grafted.annotations.filterNot(SimulatorBankProbeContext.accepts))
    GeneratedStateBankAnnotations.preflight(storage)
    val constrained = midas.passes.WriteXDCFile.execute(storage.copy(annotations = storage.annotations :+ placement))
    val constraints = constrained.annotations.collect { case a: midas.passes.XDCOutputAnnotation =>
      GoldenGateOutputFileAnnotation(a.fileBody, ".native-inplace-graft" + a.suffix.get)
    }
    require(constraints.size == 2, "native in-place graft: missing original constraints")
    def path(p: Option[String]): JValue = p.map(JString(_)).getOrElse(JNull)
    val binding = canonical(JObject(List(
      "graft_version" -> JInt(1), "scope" -> JString("existing_root_readout_adapter_only"),
      "root" -> JString(root.name), "adapter_module" -> JString(name), "adapter_instance" -> JString(instance),
      "raw_clock" -> JString(clock), "target" -> (parse(result.binding) \ "target"),
      "context_binding_sha256" -> JString(sha256(result.binding)),
      "source_sha256" -> JString(sha256(result.host.circuit.serialize)),
      "instrumented_sha256" -> JString(sha256(state.circuit.serialize)),
      "grafted_sha256" -> JString(sha256(flowed.serialize)),
      "descriptor_manifest_sha256" -> JString(descriptors.manifestSha256),
      "descriptor_binding_sha256" -> JString(descriptors.bindingSha256),
      "adapter_sha256" -> JString(sha256(rtl)), "router_sha256" -> JString(sha256(router)),
      "physical_bank_sha256" -> JString(sha256(ce(1).body)),
      "pre_link_path" -> path(placement.preLinkPath), "post_link_path" -> path(placement.postLinkPath),
      "constraint_files" -> JObject(constraints.map(f => f.fileSuffix -> JString(sha256(f.body))).toList),
      "original_execution_duplicated" -> JBool(false), "original_constraints_relocated" -> JBool(false),
      "in_place_checkpoint_widget_connected" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
    Result(grafted, Seq(GoldenGateOutputFileAnnotation(rtl, ".native-inplace-graft.sv"),
      GoldenGateOutputFileAnnotation(router, ".native-inplace-graft-router.sv"),
      GoldenGateOutputFileAnnotation(ce(1).body, ".native-inplace-graft-ce.sv"),
      GoldenGateOutputFileAnnotation(descriptors.manifest, ".native-inplace-graft-descriptors.json"),
      GoldenGateOutputFileAnnotation(binding, ".native-inplace-graft.json")) ++ constraints, binding)
  }
}
