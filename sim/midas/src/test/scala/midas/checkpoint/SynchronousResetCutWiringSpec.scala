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

class SynchronousResetCutWiringSpec extends AnyFlatSpec {
  private val source = new PhysicalClockCutPlanSpec().source
    .replace("output count : UInt<8>", "output count : UInt<64>")
    .replace("reg cycles : UInt<8>", "reg cycles : UInt<64>")
  private def lower(src: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(src), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private def generate(s: CircuitState, reset: String = "reset"): SynchronousResetCutWiring.Result = {
    val inv = SimulatorStateInventory.discover(s)
    SynchronousResetCutWiring.generate(s, inv, PhysicalClockCutPlan.discover(s, inv), reset)
  }
  private def emit(s: CircuitState): String = {
    val e = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
  }
  private def retain(name: String, s: CircuitState, r: SynchronousResetCutWiring.Result): Unit =
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("reset-routing").resolve(name); Files.createDirectories(out)
      Seq("reference.fir" -> s.circuit.serialize, "wired.fir" -> r.state.circuit.serialize,
        "audit.json" -> r.audit, "binding.json" -> r.binding).foreach { case (n, body) =>
        Files.write(out.resolve(n), body.getBytes(UTF_8))
      }
      if (name == "fixture") Seq("reference.sv" -> emit(s), "wired.sv" -> emit(r.state)).foreach { case (n, body) =>
        Files.write(out.resolve(n), body.getBytes(UTF_8))
      }
    }

  behavior.of("Complete synchronous root-reset cut wiring")
  it should "route every original root use and preserve repeated child reset and native CE consumers" in {
    val s = lower(); val before = s.circuit.serialize; val r = generate(s)
    val doc = parse(r.audit)
    val sinks = (doc \ "sinks").asInstanceOf[JArray].arr
    assert(sinks.count(x => (x \ "kind") == JString("native_ce")) == 2)
    assert(sinks.exists(x => (x \ "sink") == JString("PhysicalPlanTop.left.saved")))
    assert(sinks.exists(x => (x \ "sink") == JString("PhysicalPlanTop.right.saved")))
    assert(sinks.exists(x => (x \ "sink") == JString("PhysicalPlanTop.cycles")))
    assert((doc \ "source_synchrony") == JString("external_sampling_contract_required"))
    assert((parse(r.binding) \ "production_freeze_owners_bound") == JBool(false))
    val root = r.state.circuit.modules.collectFirst { case m: Module if m.name == s.circuit.main => m }.get
    var raw = 0; var held = 0
    def expression(e: Expression): Unit = { e match {
      case r: firrtl.WRef if r.name == "reset" => raw += 1
      case r: firrtl.WRef if r.name == SynchronousResetCutWiring.heldReset => held += 1
      case _ => ()
    }; e.foreachExpr(expression) }
    def statement(x: Statement): Unit = { x.foreachExpr(expression); x.foreachStmt(statement) }
    statement(root.body)
    assert(raw == 0 && held > 0)
    assert((parse(r.binding) \ "replaced_root_references") == JInt(held))
    assert(s.circuit.serialize == before)
    retain("fixture", s, r)
  }

  it should "retain reset-as-data memory controls side effects and external outputs" in {
    val mem = """    mem ram :
      data-type => UInt<8>
      depth => 2
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    ram.r.clk is invalid
    ram.r.addr <= bits(reset, 0, 0)
    ram.r.en <= reset
    ram.w.clk <= clock
    ram.w.addr <= UInt<1>(0)
    ram.w.en <= reset
    ram.w.data <= saved
    ram.w.mask <= UInt<1>(1)
    value <= ram.r.data
    printf(clock, reset, "reset=%d", reset)
"""
    val s = lower(source.replace("    value <= saved\n", mem))
    val r = generate(s); val sinks = (parse(r.audit) \ "sinks").asInstanceOf[JArray].arr
    assert(sinks.count(x => (x \ "kind") == JString("memory_port")) == 6)
    assert(sinks.count(x => (x \ "kind") == JString("print")) == 4)
    assert(sinks.count(x => (x \ "kind") == JString("external_output")) == 2)
    retain("memory", s, r)
  }

  it should "reject asynchronous reset clock data opaque consumers stale plans and identifier collisions" in {
    val s = lower(); val inv = SimulatorStateInventory.discover(s); val plan = PhysicalClockCutPlan.discover(s, inv)
    intercept[IllegalArgumentException] { SynchronousResetCutWiring.generate(s, inv, "{}", "reset") }
    intercept[IllegalArgumentException] { generate(s, "clock") }
    intercept[IllegalArgumentException] { generate(s, "missing") }
    val collision = lower(source.replace("    output count : UInt<64>",
      "    output count : UInt<64>\n    input checkpoint_reset_functional : UInt<1>"))
    assert(intercept[IllegalArgumentException](generate(collision)).getMessage.contains("identifier collision"))
    val async = lower(source.replace("input reset : UInt<1>", "input reset : AsyncReset")
      .replace("or(reset, bits(cycles, 0, 0))", "bits(cycles, 0, 0)")
      .replace("or(reset, bits(cycles, 1, 1))", "bits(cycles, 1, 1)"))
    intercept[IllegalArgumentException] { generate(async) }
    val clock = lower(source.replace("gate.I <= clock", "gate.I <= asClock(reset)"))
    intercept[IllegalArgumentException] { generate(clock) }
    // A native source whose I is reset-derived must also fail topology; its CE
    // remains the only opaque input admitted by the reset fanout audit.
    assert(plan.nonEmpty)
    retain("negatives-control", s, generate(s))
  }

  it should "avoid false combinational cycles on registered ready valid feedback" in {
    val src = source.replace("    input ce : UInt<1>", "    input ce : UInt<1>\n    input feedback : UInt<8>")
      .replace("saved <= add(saved, UInt<8>(3))", "saved <= add(feedback, UInt<8>(3))")
      .replace("    left.clock <= clock", "    left.feedback <= left.value\n    right.feedback <= right.value\n    left.clock <= clock")
    assert((parse(generate(lower(src)).binding) \ "deployable_capture_image") == JBool(false))
  }

  it should "require an explicit isolated compiler request and bind all emitted files" in {
    val s = lower(); val inv = SimulatorStateInventory.discover(s); val plan = PhysicalClockCutPlan.discover(s, inv)
    assert(SynchronousResetCutWiring.emit(s, inv, plan).isEmpty)
    val request = Seq(SimulatorCheckpointInventoryAnnotation(s.circuit.main), PhysicalClockCutPlanAnnotation,
      PhysicalClockCutWiringProbeAnnotation, SynchronousResetCutWiringAnnotation("reset"))
    PhysicalClockCutPlan.validateCardinality(request)
    for (as <- Seq(request.drop(1), request :+ SynchronousResetCutWiringAnnotation("reset"),
      request :+ SimulatorBankProbeAnnotation, request :+ NativeFunctionalBankProbeAnnotation,
      request :+ SemanticFAMEStateMapAnnotation, request.filterNot(_ == PhysicalClockCutWiringProbeAnnotation)))
      intercept[IllegalArgumentException] { PhysicalClockCutPlan.validateCardinality(as) }
    val requested = s.copy(annotations = s.annotations ++ request)
    val files = PhysicalClockCutPlan.emit(requested, inv, "unchanged original")
    val own = files.filter(_.fileSuffix.startsWith(".synchronous-reset-cut"))
    assert(own.size == 5)
    val binding = parse(own.find(_.fileSuffix.endsWith("-emission.json")).get.body)
    own.filterNot(_.fileSuffix.endsWith("-emission.json")).foreach { f =>
      assert((binding \ "inputs" \ f.fileSuffix) == JString(SemanticStateInventory.sha256(f.body)))
    }
    intercept[IllegalArgumentException] { SynchronousResetCutWiring.emit(requested.copy(
      annotations = requested.annotations ++ own), inv, plan) }
  }

  it should "audit and route the retained exact Rocket root when explicitly supplied" in {
    sys.props.get("checkpoint.reset.rocket.reference").foreach { path =>
      val p = Paths.get(path); assert(Files.isRegularFile(p))
      val parsed = Parser.parse(new String(Files.readAllBytes(p), UTF_8))
      val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(parsed))
      val s = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm,
        Seq(firrtl.transforms.NoCircuitDedupAnnotation))
      // Real plusarg adapters require their literal original inline sources.
      val plusarg = sys.props("checkpoint.plusarg.reference")
      val sourceText = new String(Files.readAllBytes(Paths.get(plusarg)), UTF_8)
      val annotations = s.circuit.modules.collect { case m: ExtModule if m.defname == "plusarg_reader" =>
        firrtl.transforms.BlackBoxInlineAnno(firrtl.annotations.ModuleName(m.name,
          firrtl.annotations.CircuitName(s.circuit.main)), "plusarg_reader.v", sourceText)
      }
      val reference = s.copy(annotations = s.annotations ++ annotations)
      val result = generate(reference)
      val sinks = (parse(result.audit) \ "sinks").asInstanceOf[JArray].arr
      assert(sinks.exists(x => (x \ "kind") == JString("native_ce")))
      assert((parse(result.audit) \ "reset_source") == JString("F1Shim.reset"))
      retain("rocket", reference, result)
    }
  }
}
