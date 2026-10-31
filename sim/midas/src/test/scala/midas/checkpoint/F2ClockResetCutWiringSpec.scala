// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class F2ClockResetCutWiringSpec extends AnyFlatSpec {
  private val source = new PhysicalClockCutPlanSpec().source
    .replace("output count : UInt<8>", "output count : UInt<64>")
    .replace("reg cycles : UInt<8>", "reg cycles : UInt<64>")
  private def lower(src: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(src), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private def generate(s: CircuitState, cycle: String = "count", owners: Int = 4): F2ClockResetCutWiring.Result = {
    val inv = SimulatorStateInventory.discover(s)
    F2ClockResetCutWiring.generate(s, inv, PhysicalClockCutPlan.discover(s, inv), "reset", cycle, owners)
  }
  private def emit(s: CircuitState): String = {
    val e = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
  }
  behavior.of("Compiler-connected F2 common-cut collector")
  it should "own mask and held reset internally and leave the raw collector clock ungated" in {
    val s = lower(); val original = s.circuit.serialize; val r = generate(s)
    val root = r.state.circuit.modules.collectFirst { case m: Module if m.name == s.circuit.main => m }.get
    assert(!root.ports.exists(p => Seq(PhysicalClockCutWiring.mask, SynchronousResetCutWiring.heldReset).contains(p.name)))
    val text = root.body.serialize
    assert(text.contains("checkpoint_f2_adapter.clock <= clock"))
    assert(text.contains("checkpoint_f2_adapter.target_cycle <= count"))
    assert(text.contains("checkpoint_physical_mask <= checkpoint_f2_adapter.clock_mask"))
    assert(text.contains("checkpoint_reset_functional <= checkpoint_f2_adapter.functional_reset"))
    assert(text.contains("checkpoint_f2_adapter.owner_present <= cat(eq(reset,"))
    assert(text.contains("checkpoint_f2_owner_present)"))
    val ext = r.state.circuit.modules.collectFirst { case m: ExtModule if m.defname == F2ClockResetCutWiring.definition => m }.get
    assert(ext.params == Seq(IntParam("GATES", 2), IntParam("OWNERS", 5)))
    assert(root.ports.find(_.name == "checkpoint_f2_control_state").get.tpe == UIntType(IntWidth(81)))
    assert((parse(r.binding) \ "target_time_authority_proven") == JBool(false))
    assert((parse(r.binding) \ "deployable_capture_image") == JBool(false))
    assert(s.circuit.serialize == original)
    // Retain compiler output, never substitute handwritten wiring in native/EDA checks.
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("f2-collector").resolve("fixture"); Files.createDirectories(out)
      Seq("reference.fir" -> original, "reference.sv" -> emit(s), "wired.fir" -> r.state.circuit.serialize,
        "wired.sv" -> emit(r.state), "audit.json" -> r.reset.audit, "binding.json" -> r.binding,
        "reset-binding.json" -> r.reset.binding).foreach { case (n, text) => Files.write(out.resolve(n), text.getBytes(UTF_8)) }
    }
  }
  it should "reject external undriven reshaped and missing cycle sources without guessing target time" in {
    val s = lower()
    for (name <- Seq("reset", "clock", "missing", "left.value", "cycles"))
      assert(intercept[IllegalArgumentException](generate(s, name)).getMessage.contains("driven scalar UInt<64> root output"))
    assert(intercept[IllegalArgumentException](generate(lower(source.replace("output count : UInt<64>", "output count : UInt<63>"))))
      .getMessage.contains("driven scalar UInt<64> root output"))
    for (owners <- Seq(-1, 0, 65537)) intercept[IllegalArgumentException] { generate(s, owners = owners) }
  }
  it should "reject collector namespace and adapter-definition collisions" in {
    val collision = lower(source.replace("    output count : UInt<64>",
      "    output count : UInt<64>\n    input checkpoint_f2_owner_present : UInt<4>"))
    assert(intercept[IllegalArgumentException](generate(collision)).getMessage.contains("identifier collision"))
    val s = lower()
    val ext = ExtModule(NoInfo, F2ClockResetCutWiring.definition, Nil, F2ClockResetCutWiring.definition, Nil)
    assert(intercept[IllegalArgumentException](generate(s.copy(circuit = s.circuit.copy(modules = s.circuit.modules :+ ext))))
      .getMessage.contains("identifier collision"))
    assert(intercept[IllegalArgumentException](generate(s.copy(circuit = s.circuit.copy(modules =
      s.circuit.modules :+ ext.copy(name = "OtherName"))))).getMessage.contains("identifier collision"))
  }
  it should "require the explicit isolated request and hash every separately emitted file" in {
    val s = lower(); val inv = SimulatorStateInventory.discover(s); val plan = PhysicalClockCutPlan.discover(s, inv)
    assert(F2ClockResetCutWiring.emit(s, inv, plan).isEmpty)
    val request = Seq(SimulatorCheckpointInventoryAnnotation(s.circuit.main), PhysicalClockCutPlanAnnotation,
      PhysicalClockCutWiringProbeAnnotation, SynchronousResetCutWiringAnnotation("reset"), F2ClockResetCutWiringAnnotation("count", 4))
    PhysicalClockCutPlan.validateCardinality(request)
    for (as <- Seq(request.drop(1), request :+ request.last, request.dropRight(2) :+ request.last,
      request :+ SimulatorBankProbeAnnotation, request :+ NativeFunctionalBankProbeAnnotation,
      request.dropRight(1) :+ F2ClockResetCutWiringAnnotation("count", 0)))
      intercept[IllegalArgumentException] { PhysicalClockCutPlan.validateCardinality(as) }
    val requested = s.copy(annotations = s.annotations ++ request)
    val files = PhysicalClockCutPlan.emit(requested, inv, "unchanged original")
    val own = files.filter(_.fileSuffix.startsWith(".f2-clock-reset-cut")); assert(own.size == 4)
    val binding = parse(own.find(_.fileSuffix.endsWith("-emission.json")).get.body)
    own.filterNot(_.fileSuffix.endsWith("-emission.json")).foreach { f =>
      assert((binding \ "inputs" \ f.fileSuffix) == JString(SemanticStateInventory.sha256(f.body)))
    }
    intercept[IllegalArgumentException] { F2ClockResetCutWiring.emit(requested.copy(annotations = requested.annotations ++ own), inv, plan) }
    intercept[IllegalArgumentException] { generate(s, owners = 0) }
    intercept[IllegalArgumentException] { F2ClockResetCutWiring.generate(s, inv, "{}", "reset", "count", 4) }
  }
}
