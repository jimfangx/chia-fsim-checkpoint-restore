// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.DontTouchAnnotation
import firesim.lib.bridgeutils.RationalClock
import midas.passes.fame._
import org.scalatest.flatspec.AnyFlatSpec

class FAMEClockProgressSpec extends AnyFlatSpec {
  private val source = """circuit Top :
  module Model :
    input clockA : Clock
    input clockB : Clock
    input data : UInt<8>
    output clockOutA : Clock
    output clockOutB : Clock
    output value : UInt<8>
    output slow : UInt<8>
    reg a : UInt<8>, clockA
    reg b : UInt<8>, clockB
    a <= add(a, data)
    b <= add(b, UInt<8>(1))
    value <= a
    slow <= b
    clockOutA <= clockA
    clockOutB <= clockB
  module Top :
    input hostClock : Clock
    input hostReset : UInt<1>
    input clockA : Clock
    input clockB : Clock
    input data : UInt<8>
    output clockOutA : Clock
    output clockOutB : Clock
    output value : UInt<8>
    output slow : UInt<8>
    inst model of Model
    model.clockA <= clockA
    model.clockB <= clockB
    model.data <= data
    value <= model.value
    slow <= model.slow
    clockOutA <= model.clockOutA
    clockOutB <= model.clockOutB
"""
  private def seed(): CircuitState = {
    val state = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm))
    val top = ModuleTarget("Top", "Top"); val model = ModuleTarget("Top", "Model")
    val clockInfo = TargetClockChannel(Seq(RationalClock("fast", 1, 1), RationalClock("slow", 1, 3)), Seq(1, 3))
    state.copy(form = LowForm, annotations = Seq(
      FAMEHostClock(top.ref("hostClock")), FAMEHostReset(top.ref("hostReset")), FAMETransformAnnotation(model),
      FAMEChannelConnectionAnnotation.sink("clocks", clockInfo, None, Seq(top.ref("clockA"), top.ref("clockB"))),
      FAMEChannelConnectionAnnotation.sink("data", PipeChannel(0), Some(top.ref("clockOutA")), Seq(top.ref("data"))),
      FAMEChannelConnectionAnnotation.source("value", PipeChannel(0), Some(top.ref("clockOutA")), Seq(top.ref("value"))),
      FAMEChannelConnectionAnnotation.source("slow", PipeChannel(0), Some(top.ref("clockOutB")), Seq(top.ref("slow"))),
      FAMEChannelPortsAnnotation("clocks", None, Seq(model.ref("clockA"), model.ref("clockB"))),
      FAMEChannelPortsAnnotation("data", Some(model.ref("clockOutA")), Seq(model.ref("data"))),
      FAMEChannelPortsAnnotation("value", Some(model.ref("clockOutA")), Seq(model.ref("value"))),
      FAMEChannelPortsAnnotation("slow", Some(model.ref("clockOutB")), Seq(model.ref("slow")))))
  }
  private def transform(observe: Boolean): CircuitState = {
    val s = seed()
    val requested = if (!observe) s else s.copy(annotations = s.annotations ++ Seq(
      SimulatorCheckpointInventoryAnnotation("Top"), FAMEClockProgressAnnotation))
    val fame = new FAMETransform().execute(requested)
    // The normal downstream native gate pass, never a handwritten replacement.
    midas.passes.xilinx.HostSpecialization.execute(midas.passes.DefineAbstractClockGate.runTransform(fame))
  }
  private def emit(s: CircuitState): (CircuitState, String) = {
    val e = new SemanticCleanEmitter
    val lowered = new Compiler(Forms.LowForm).execute(s.copy(form = HighForm,
      annotations = s.annotations.collect { case a: DontTouchAnnotation => a }))
    val result = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      lowered.copy(annotations = lowered.annotations :+
        EmitCircuitAnnotation(e.getClass)))
    (e.preparedState.get, result.getEmittedCircuit.value)
  }
  // Probe outputs are a verification sibling only. Production uses the same
  // internal registers via generated state banks; no hierarchical HDL reads.
  private def probes(s: CircuitState): CircuitState = {
    val m = s.circuit.modules.collectFirst { case m: Module if m.name == "Model" => m }.get
    val declarations = scala.collection.mutable.ArrayBuffer.empty[(String, Type)]
    def visit(x: Statement): Unit = {
      x match {
        case r: DefRegister if r.name.startsWith(FAMEClockProgress.prefix) => declarations += r.name -> r.tpe
        case w: DefWire if w.name.startsWith(FAMEClockProgress.prefix) => declarations += w.name -> w.tpe
        case _ => ()
      }
      x.foreachStmt(visit)
    }
    visit(m.body)
    val ps = declarations.map { case (n, t) => Port(NoInfo, "probe_" + n, Output, t) }.toSeq
    s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case x: Module if x.name == "Model" => x.copy(ports = x.ports ++ ps,
        body = Block(Seq(x.body) ++ declarations.map { case (n, t) => Connect(NoInfo, WRef("probe_" + n, t), WRef(n, t)) }))
      case x: Module if x.name == "Top" => x.copy(ports = x.ports ++ ps,
        body = Block(Seq(x.body) ++ ps.map(p => Connect(NoInfo, WRef(p), WSubField(WRef("model"), p.name, p.tpe)))))
      case x => x
    }))
  }
  behavior.of("FAME construction-site clock progress")
  it should "observe actual clock consumption and delayed native launches without changing original rules" in {
    val reference = transform(false); val observed = transform(true)
    assert(!observed.annotations.contains(FAMEClockProgressAnnotation))
    val m = observed.circuit.modules.collectFirst { case m: Module if m.name == "Model" => m }.get
    val original = reference.circuit.modules.collectFirst { case m: Module if m.name == "Model" => m }.get
    def leaves(s: Statement): Seq[Statement] = s match {
      case b: Block => b.stmts.flatMap(leaves)
      case EmptyStmt => Nil
      case x => Seq(x)
    }
    val functional = leaves(m.body).filterNot {
      case d: IsDeclaration => d.name.startsWith(FAMEClockProgress.prefix)
      case c: Connect => c.loc.serialize.startsWith(FAMEClockProgress.prefix)
      case _ => false
    }
    assert(functional.map(_.serialize) == leaves(original.body).map(_.serialize))
    val body = m.body.serialize
    assert(body.contains("checkpoint_fame_clockA_pending <= clockA_enabled"))
    assert(body.contains("checkpoint_fame_clockA_launch <= and(and(clockA_enabled, targetCycleFinishing), not(hostReset))"))
    assert(body.contains("checkpoint_fame_clockA_consume <= and(and(targetCycleFinishing, not(hostReset)), asUInt(clocks_sink.bits.A))"))
    assert(observed.annotations.collect { case a: DontTouchAnnotation if a.target.ref.startsWith(FAMEClockProgress.prefix) => a }.size == 12)
    val (actual, rtl) = emit(observed); val (_, originalRTL) = emit(reference)
    val inventory = SimulatorStateInventory.discover(actual)
    assert(inventory.contains("checkpoint_fame_clockA_applied"))
    assert(inventory.contains("checkpoint_fame_clockB_consumed"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-progress"); Files.createDirectories(out)
      Seq("reference.fir" -> reference.circuit.serialize, "observed.fir" -> observed.circuit.serialize,
        "observed.sv" -> rtl, "reference.sv" -> originalRTL, "probed.sv" -> emit(probes(observed))._2,
        "state.json" -> inventory).foreach {
        case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8))
      }
    }
  }
  it should "leave unrequested builds byte-identical and reject incomplete duplicate requests" in {
    val s = seed(); assert(!FAMEClockProgress.requested(s))
    assert(new FAMETransform().execute(s).circuit.serialize == new FAMETransform().execute(s).circuit.serialize)
    for (as <- Seq(Seq(FAMEClockProgressAnnotation), Seq(FAMEClockProgressAnnotation, FAMEClockProgressAnnotation,
      SimulatorCheckpointInventoryAnnotation("Top"))))
      intercept[IllegalArgumentException](new FAMETransform().execute(s.copy(annotations = s.annotations ++ as)))
    intercept[IllegalArgumentException](new FAMETransform().execute(s.copy(annotations =
      s.annotations.filterNot(_.isInstanceOf[FAMETransformAnnotation]) ++ Seq(
        SimulatorCheckpointInventoryAnnotation("Top"), FAMEClockProgressAnnotation))))
  }
  it should "reject unresolved domains and reserved names rather than hiding observer state" in {
    val s = seed(); val m = s.circuit.modules.collectFirst { case m: Module if m.name == "Model" => m }.get
    val clock = WRef("hostClock", ClockType); val bit = WRef("flag", UIntType(IntWidth(1)))
    val domain = FAMEClockProgress.Domain("clockA", bit, bit, bit)
    def instrument(module: Module = m, ds: Seq[FAMEClockProgress.Domain] = Seq(domain)) =
      FAMEClockProgress.instrument("Top", module, clock, bit, bit, ds)
    for (ds <- Seq(Nil, Seq(domain, domain), Seq(domain.copy(incoming = clock)), Seq(domain.copy(name = "a.b"))))
      intercept[IllegalArgumentException](instrument(ds = ds))
    intercept[IllegalArgumentException](instrument(m.copy(ports = m.ports :+ Port(NoInfo, "checkpoint_fame_external", Input, bit.tpe))))
    intercept[IllegalArgumentException](instrument(m.copy(body = Block(Seq(m.body, DefWire(NoInfo, "checkpoint_fame_hidden", bit.tpe))))))
  }
}
