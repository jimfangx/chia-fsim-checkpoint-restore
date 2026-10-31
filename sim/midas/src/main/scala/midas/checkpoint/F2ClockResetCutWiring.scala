// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

case class F2ClockResetCutWiringAnnotation(cyclePort: String, owners: Int)
    extends NoTargetAnnotation with Unserializable with HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-f2-clock-reset-cut",
    toAnnotationSeq = value => {
      val parts = value.split(":", -1)
      require(parts.length == 2 && parts(1).matches("[1-9][0-9]*"),
        "F2 cut requires TARGET_CYCLE_OUTPUT:OWNER_COUNT")
      Seq(F2ClockResetCutWiringAnnotation(parts(0), parts(1).toInt))
    },
    helpText = "Compose the F2 collector inside a separate reset/clock sibling; requires TARGET_CYCLE_OUTPUT:OWNER_COUNT, no image admission.",
  ))
}

/** Connect the existing synthesized adapter in the compiler, rather than in a
  * handwritten external fixture. Appending the collector AFTER clock rewriting
  * keeps its clock ungated. Its mask and held-reset nets become internal wires;
  * nobody outside the root can independently drive either half of the cut.
  *
  * External shell writers, collector initialization and every owner remain
  * mandatory inputs, without default acknowledgements. Selecting a driven
  * 64-bit output proves connectivity, not that it is authoritative target time.
  * This isolated sibling does not install a production shell/transport or waive
  * the ordinary compiler's physical/native ownership guards.
  */
object F2ClockResetCutWiring {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_f2_"
  val adapter = prefix + "adapter"
  val definition = "CheckpointF2ClockResetCut"
  case class Result(state: CircuitState, reset: SynchronousResetCutWiring.Result, binding: String)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException("F2 clock/reset cut wiring unsupported: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  def validateRequest(as: firrtl.AnnotationSeq): Unit = {
    val requests = as.collect { case a: F2ClockResetCutWiringAnnotation => a }
    if (requests.isEmpty) return
    if (requests.size != 1 || !requests.head.cyclePort.matches("[A-Za-z_][A-Za-z0-9_$]*") ||
        requests.head.owners < 1 || requests.head.owners > 65536 ||
        as.count(_.isInstanceOf[SynchronousResetCutWiringAnnotation]) != 1)
      fail("one explicit driven cycle output, 1..65536 owners and one reset-cut request required")
    SynchronousResetCutWiring.validateRequest(as)
  }

  def generate(input: CircuitState, manifest: String, plan: String, resetPort: String,
      cyclePort: String, owners: Int): Result = {
    if (owners < 1 || owners > 65536) fail("owner count outside 1..65536")
    val reset = SynchronousResetCutWiring.generate(input, manifest, plan, resetPort)
    val root = reset.state.circuit.modules.collectFirst { case m: Module if m.name == input.circuit.main => m }.get
    val declarations = root.ports.map(_.name) ++ all(root.body).collect { case d: IsDeclaration => d.name }
    if (declarations.exists(_.startsWith(prefix)) || reset.state.circuit.modules.exists {
      case m: ExtModule => m.name == definition || m.defname == definition
      case m => m.name == definition
    })
      fail("reserved collector identifier collision")
    if (!root.ports.exists(p => p.name == cyclePort && p.direction == Output && p.tpe == UIntType(IntWidth(64))) ||
        !all(root.body).exists { case c: Connect => c.loc.serialize == cyclePort; case _ => false })
      fail("cycle source must be an existing driven scalar UInt<64> root output")
    val gates = (parse(plan) \ "physical_gates").asInstanceOf[JArray].arr.size
    val clock = (parse(plan) \ "common_source_clock").asInstanceOf[JString].s
    def uint(width: Int): Type = UIntType(IntWidth(width))
    def port(name: String, direction: Direction, width: Int): Port = Port(NoInfo, name, direction, uint(width))
    val inputs = Seq("controller_reset" -> 1, "freeze_request" -> 1, "rst_main_n" -> 1,
      "ddr_ready" -> 1, "writer_state" -> 4, "owner_present" -> owners)
    val outputs = Seq("stopped" -> 1, "read_enable" -> 1, "failed" -> 1, "captured_ce" -> gates,
      "captured_writers" -> 4, "cut_cycle" -> 64, "physical_error" -> 3, "reset_error" -> 2,
      "control_state" -> (gates + 79))
    val exported = outputs ++ Seq("clock_mask" -> 1, "functional_reset" -> 1)
    val ext = ExtModule(NoInfo, definition,
      Seq(Port(NoInfo, "clock", Input, ClockType)) ++ inputs.map { case (n, w) =>
        port(n, Input, if (n == "owner_present") w + 1 else w)
      } ++
        Seq(port("logical_ce", Input, gates), port("target_cycle", Input, 64),
          port("functional_reset", Output, 1), port("clock_mask", Output, 1)) ++
        outputs.map { case (n, w) => port(n, Output, w) },
      definition, Seq(IntParam("GATES", gates), IntParam("OWNERS", owners + 1)))
    def pin(name: String): Expression = WSubField(WRef(adapter), name)
    val internal = Seq(PhysicalClockCutWiring.mask, SynchronousResetCutWiring.heldReset)
    val before = Seq(WDefInstance(adapter, definition)) ++
      internal.map(n => DefWire(NoInfo, n, uint(1)))
    // Keep the original raw reset connected to a real owner check. A shell
    // connection inconsistent with the literal writer expression must cancel,
    // rather than leave the old root reset silently unused after composition.
    def writerBit(bit: Int): Expression = DoPrim(firrtl.PrimOps.Bits,
      Seq(WRef(prefix + "writer_state")), Seq(BigInt(bit), BigInt(bit)), uint(1))
    val writerReset = DoPrim(firrtl.PrimOps.Not,
      Seq(DoPrim(firrtl.PrimOps.And, Seq(writerBit(1), writerBit(3)), Nil, uint(1))), Nil, uint(1))
    val resetConsistent = DoPrim(firrtl.PrimOps.Eq, Seq(WRef(resetPort), writerReset), Nil, uint(1))
    val checkedOwners = DoPrim(firrtl.PrimOps.Cat,
      Seq(resetConsistent, WRef(prefix + "owner_present")), Nil, uint(owners + 1))
    val connections = Seq(Connect(NoInfo, pin("clock"), WRef(clock)),
      Connect(NoInfo, pin("logical_ce"), WRef(PhysicalClockCutWiring.ce)),
      Connect(NoInfo, pin("target_cycle"), WRef(cyclePort)),
      Connect(NoInfo, WRef(internal.head), pin("clock_mask")),
      Connect(NoInfo, WRef(internal(1)), pin("functional_reset")),
      Connect(NoInfo, pin("owner_present"), checkedOwners)) ++
      inputs.filterNot(_._1 == "owner_present").map { case (n, _) => Connect(NoInfo, pin(n), WRef(prefix + n)) } ++
      exported.map { case (n, _) => Connect(NoInfo, WRef(prefix + n), pin(n)) }
    val finalRoot = root.copy(ports = root.ports.filterNot(p => internal.contains(p.name)) ++
      inputs.map { case (n, w) => port(prefix + n, Input, w) } ++
      exported.map { case (n, w) => port(prefix + n, Output, w) },
      body = Block(before ++ Seq(root.body) ++ connections))
    val circuit = reset.state.circuit.copy(modules = reset.state.circuit.modules.map {
      case m if m.name == root.name => finalRoot
      case m => m
    } :+ ext)
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val state = reset.state.copy(circuit = flowed)
    val binding = canonical(JObject(List("f2_cut_wiring_version" -> JInt(1),
      "scope" -> JString("compiler_connected_f2_collector_sibling_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "simulator_manifest_sha256" -> JString(sha256(manifest)), "plan_sha256" -> JString(sha256(plan)),
      "reset_wiring_sha256" -> JString(sha256(reset.binding)),
      "wired_reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "root_module" -> JString(root.name), "adapter" -> JString(root.name + "." + adapter),
      "ungated_collector_clock" -> JString(root.name + "." + clock),
      "cycle_source" -> JString(root.name + "." + cyclePort), "gates" -> JInt(gates), "owners" -> JInt(owners),
      "raw_reset_consistency_owner" -> JString(root.name + "." + resetPort),
      "shell_writer_bit_order" -> JArray(Seq("pre_sync_rst_n_firesim", "rst_firesim_n_sync",
        "ddr_ready_pre_sync_meta", "ddr_ready_sync").map(JString(_)).toList),
      "external_inputs" -> JArray(inputs.map { case (n, _) => JString(prefix + n) }.toList),
      "target_time_authority_proven" -> JBool(false), "production_shell_installed" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "collector_state_atomically_owned" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false))))
    Result(state, reset, binding)
  }

  def emit(input: CircuitState, manifest: String, plan: String): Seq[GoldenGateOutputFileAnnotation] = {
    validateRequest(input.annotations)
    val requests = input.annotations.collect { case a: F2ClockResetCutWiringAnnotation => a }
    if (requests.isEmpty) return Nil
    if (input.annotations.exists {
      case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(".f2-clock-reset-cut")
      case _ => false
    }) fail("existing F2 cut output")
    val request = requests.head
    val reset = input.annotations.collect { case a: SynchronousResetCutWiringAnnotation => a }.head
    val result = generate(input, manifest, plan, reset.resetPort, request.cyclePort, request.owners)
    val e = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      result.state.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
    val files = Seq(".f2-clock-reset-cut.fir" -> result.state.circuit.serialize,
      ".f2-clock-reset-cut.sv" -> emitted, ".f2-clock-reset-cut-binding.json" -> result.binding)
    files.map { case (n, body) => GoldenGateOutputFileAnnotation(body, n) } :+
      GoldenGateOutputFileAnnotation(canonical(JObject(List("complete_fpga_capture" -> JBool(false),
        "deployable_capture_image" -> JBool(false),
        "inputs" -> JObject(files.map { case (n, body) => n -> JString(sha256(body)) }.toList)))),
        ".f2-clock-reset-cut-emission.json")
  }
}
