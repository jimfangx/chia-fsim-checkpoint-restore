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

/** Independent FAME-transform fixture shared by witness/attachment tests. */
object FAMEClockCutWitnessFixture {
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
  def transform(observe: Boolean): CircuitState = {
    val s = seed()
    val requested = if (!observe) s else s.copy(annotations = s.annotations ++ Seq(
      SimulatorCheckpointInventoryAnnotation("Top"), FAMEClockProgressAnnotation))
    val fame = new FAMETransform().execute(requested)
    // The normal downstream native gate pass, never a handwritten replacement.
    midas.passes.xilinx.HostSpecialization.execute(midas.passes.DefineAbstractClockGate.runTransform(fame))
  }
  def emit(s: CircuitState): (CircuitState, String) = {
    val e = new SemanticCleanEmitter
    val lowered = new Compiler(Forms.LowForm).execute(s.copy(form = HighForm,
      annotations = s.annotations.filter {
        case _: DontTouchAnnotation | _: FAMEClockProgressSignalAnnotation | _: FAMEClockProgressModelAnnotation => true
        case _ => false
      }))
    val result = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
      lowered.copy(annotations = lowered.annotations :+
        EmitCircuitAnnotation(e.getClass)))
    (e.preparedState.get, result.getEmittedCircuit.value)
  }
  def prepared(): CircuitState = {
    val s = emit(transform(true))._1
    s.copy(form = LowForm, annotations = s.annotations.filterNot(_.isInstanceOf[EmitCircuitAnnotation]))
  }
}

class FAMEClockCutWitnessSpec extends AnyFlatSpec {
  import org.json4s._
  import org.json4s.native.JsonMethods.parse
  import SemanticStateInventory.{canonical, sha256}
  private def prepare() = {
    val s = FAMEClockCutWitnessFixture.prepared()
    val inv = SimulatorStateInventory.discover(s)
    (s, inv, PhysicalClockCutPlan.discover(s, inv))
  }
  private def all(s: Statement): Seq[Statement] = {
    val xs = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { xs += x; x.foreachStmt(visit) }
    visit(s); xs.toSeq
  }
  behavior.of("Reusable FAME combinational cut witness")

  it should "preserve original state and gates while adding only the checked local predicate and applied label" in {
    val (s, inv, plan) = prepare()
    val original = s.circuit.serialize
    val r = FAMEClockCutWiring.generateWitness(s, inv, plan, "clockA")
    FAMEClockCutWiring.validateWitness(s, inv, plan, "clockA", r)
    FAMEClockCutWiring.validateWitness(s, "clockA", r)
    val roleOnly = FAMEClockCutWiring.generateWitness(s, "clockA")
    assert(roleOnly.state.circuit.serialize == r.state.circuit.serialize && roleOnly.binding == r.binding)
    val root = r.state.circuit.modules.collectFirst { case m: Module if m.name == "Top" => m }.get
    assert(root.ports.takeRight(2).map(_.name) == Seq(FAMEClockCutWiring.valid, FAMEClockCutWiring.cycle))
    val witnessInventory = SimulatorStateInventory.discover(r.state)
    val witnessPlan = parse(PhysicalClockCutPlan.discover(r.state, witnessInventory))
    // Reopening prepared connectors intentionally renames their existing local
    // clock wires. Authenticate every StateID/shape/reset and each consumer's
    // resolved root/gate chain, rather than equating those incidental aliases.
    def withoutClockAlias(row: JValue): JValue = row match {
      case JObject(fields) => JObject(fields.filterNot(_._1 == "clock_expression"))
      case _ => fail("expected state/clock-consumer object")
    }
    def rows(doc: JValue, key: String): Seq[JValue] = (doc \ key).asInstanceOf[JArray].arr
    assert(rows(parse(witnessInventory), "state").map(withoutClockAlias) ==
      rows(parse(inv), "state").map(withoutClockAlias))
    assert(rows(witnessPlan, "sequential_consumers").map(withoutClockAlias) ==
      rows(parse(plan), "sequential_consumers").map(withoutClockAlias))
    assert((witnessPlan \ "state_clock_coverage") == (parse(plan) \ "state_clock_coverage"))
    assert((witnessPlan \ "common_source_clock") == (parse(plan) \ "common_source_clock"))
    def regs(state: CircuitState) = state.circuit.modules.collect { case m: Module =>
      m.name -> all(m.body).collect { case x: DefRegister =>
        // Exact register declarations, including resets, are compared after
        // their independently checked clock origins above are abstracted.
        x.copy(clock = WRef("verified_clock_origin", ClockType)).serialize
      }
    }.toMap
    assert(regs(r.state) == regs(s))
    def gates(state: CircuitState) = GeneratedExternalOwners.instances(state, state.circuit.main)
      .collect { case (path, m: ExtModule) if m.defname == "BUFGCE" => path }
    assert(gates(r.state) == gates(s))
    assert(r.state.circuit.modules.map(_.name) == s.circuit.modules.map(_.name))
    val text = r.state.circuit.serialize
    assert(!text.contains("checkpoint_f2_adapter") && !text.contains("checkpoint_physical_root_gate"))
    assert(text.contains("checkpoint_fame_cut_applied_cycle <= checkpoint_fame_clockA_applied"))
    assert(text.contains("eq(checkpoint_fame_clockB_consumed, add(checkpoint_fame_clockB_applied, clockB_enabled))"))
    val binding = parse(r.binding)
    assert((binding \ "label_role") == JString("applied"))
    assert((binding \ "signals").asInstanceOf[JArray].arr.size == 21)
    assert((binding \ "new_storage_bits") == JInt(0))
    Seq("global_target_cut_proven", "production_freeze_owners_bound", "complete_fpga_capture",
      "deployable_capture_image", "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
    assert(s.circuit.serialize == original)
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-witness"); Files.createDirectories(out)
      val e = new SemanticCleanEmitter
      val rtl = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
        r.state.copy(annotations = Seq(EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
      val files = Seq("reference.fir" -> s.circuit.serialize,
        "reference.sv" -> FAMEClockCutWitnessFixture.emit(FAMEClockCutWitnessFixture.transform(false))._2,
        "wired.fir" -> r.state.circuit.serialize, "wired.sv" -> rtl,
        "binding.json" -> r.binding, "manifest.json" -> inv, "plan.json" -> plan)
      files.foreach { case (n, body) => Files.write(out.resolve(n), body.getBytes(UTF_8)) }
      Files.write(out.resolve("emission.json"), canonical(JObject(List(
        "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
        "inputs" -> JObject(files.map { case (n, body) => n -> JString(sha256(body)) }.toList)
      ))).getBytes(UTF_8))
    }
  }

  it should "share exactly the same unmasked witness with native bank composition" in {
    val (s, inv, plan) = prepare()
    val w = FAMEClockCutWiring.generateWitness(s, inv, plan, "clockA")
    val native = FAMEClockCutWiring.generateNative(s, inv, plan, "hostReset", "clockA", 3)
    assert(native.native.get.witness.circuit.serialize == w.state.circuit.serialize)
    assert(native.native.get.witness.annotations.toSeq == w.state.annotations.toSeq)
    assert((parse(native.binding) \ "reopened_reference_sha256") == (parse(w.binding) \ "reopened_reference_sha256"))
    assert((parse(native.binding) \ "label_path") == (parse(w.binding) \ "label_path"))
    val slow = FAMEClockCutWiring.generateWitness(s, inv, plan, "clockB")
    assert((parse(slow.binding) \ "primary_domain") == JString("clockB"))
    assert(slow.state.circuit.serialize.contains("checkpoint_fame_cut_applied_cycle <= checkpoint_fame_clockB_applied"))
  }

  it should "reject stale sources missing roles replicated models and reserved identifiers" in {
    val (s, inv, plan) = prepare()
    for ((i,p,d) <- Seq((inv + " ", plan, "clockA"), (inv, plan + " ", "clockA"), (inv, plan, "missing"))) {
      intercept[IllegalArgumentException] { FAMEClockCutWiring.generateWitness(s, i, p, d) }
    }
    val changed = Seq(s.copy(form = HighForm),
      s.copy(annotations = s.annotations.filterNot { case a: FAMEClockProgressSignalAnnotation => a.role == "overflow"; case _ => false }),
      s.copy(annotations = s.annotations ++ s.annotations.collect { case a: FAMEClockProgressModelAnnotation => a }))
    changed.foreach { bad => intercept[IllegalArgumentException] {
      val badInv = SimulatorStateInventory.discover(bad)
      FAMEClockCutWiring.generateWitness(bad, badInv, PhysicalClockCutPlan.discover(bad, badInv), "clockA")
    } }
    val collision = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m: Module if m.name == "Top" => m.copy(ports = m.ports :+ Port(NoInfo, FAMEClockCutWiring.valid, Input, UIntType(IntWidth(1))))
      case m => m
    }))
    val ci = SimulatorStateInventory.discover(collision)
    intercept[IllegalArgumentException] {
      FAMEClockCutWiring.generateWitness(collision, ci, PhysicalClockCutPlan.discover(collision, ci), "clockA")
    }
  }

  it should "reject coherently rehashed witness rewiring and changed compiler metadata" in {
    val (s, inv, plan) = prepare()
    val r = FAMEClockCutWiring.generateWitness(s, inv, plan, "clockA")
    for (sink <- Seq(FAMEClockCutWiring.valid, FAMEClockCutWiring.cycle)) {
      var count = 0
      def rewrite(x: Statement): Statement = x match {
        case c: Connect if c.loc.serialize == sink =>
          count += 1
          c.copy(expr = UIntLiteral(0, IntWidth(if (sink == FAMEClockCutWiring.valid) 1 else 64)))
        case x => x.mapStmt(rewrite)
      }
      val circuit = r.state.circuit.copy(modules = r.state.circuit.modules.map {
        case m: Module if m.name == "Top" => m.copy(body = rewrite(m.body))
        case m => m
      })
      assert(count == 1)
      val binding = canonical(parse(r.binding).transformField {
        case ("wired_reference_sha256", _) => "wired_reference_sha256" -> JString(sha256(circuit.serialize))
      })
      intercept[IllegalArgumentException] {
        FAMEClockCutWiring.validateWitness(s, inv, plan, "clockA", r.copy(state = r.state.copy(circuit = circuit), binding = binding))
      }
    }
    val rename = firrtl.RenameMap()
    rename.record(ModuleTarget("Top", "Top").ref("data"), ModuleTarget("Top", "Top").ref("corrupt"))
    Seq(r.copy(binding = r.binding + " "), r.copy(state = r.state.copy(form = HighForm)),
      r.copy(state = r.state.copy(annotations = r.state.annotations.drop(1))),
      r.copy(state = r.state.copy(renames = Some(rename)))).foreach { bad =>
      intercept[IllegalArgumentException] { FAMEClockCutWiring.validateWitness(s, inv, plan, "clockA", bad) }
    }
  }
}
