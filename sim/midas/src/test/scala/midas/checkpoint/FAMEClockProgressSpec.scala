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
      annotations = s.annotations.filter {
        case _: DontTouchAnnotation | _: FAMEClockProgressSignalAnnotation | _: FAMEClockProgressModelAnnotation => true
        case _ => false
      }))
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
  it should "bind the applied-edge label and checked local owner into the generated raw-clock collector" in {
    val state = emit(transform(true))._1.copy(form = LowForm)
    val inv = SimulatorStateInventory.discover(state)
    val plan = PhysicalClockCutPlan.discover(state, inv)
    val result = FAMEClockCutWiring.generate(state, inv, plan, "hostReset", "clockA", 3)
    FAMEClockCutWiring.validate(state, inv, plan, "hostReset", "clockA", 3, result)
    val root = result.state.circuit.modules.collectFirst { case m: Module if m.name == "Top" => m }.get
    val text = result.state.circuit.serialize
    assert(root.ports.find(_.name == "checkpoint_f2_owner_present").get.tpe == UIntType(IntWidth(3)))
    assert(text.contains("checkpoint_f2_adapter.target_cycle <= checkpoint_fame_cut_applied_cycle"))
    assert(text.contains("cat(checkpoint_fame_cut_consistent, checkpoint_f2_owner_present)"))
    assert(text.contains("checkpoint_fame_cut_applied_cycle <= checkpoint_fame_clockA_applied"))
    assert(text.contains("eq(checkpoint_fame_clockB_consumed, add(checkpoint_fame_clockB_applied, clockB_enabled))"))
    assert(text.contains("checkpoint_f2_adapter.clock <= hostClock"))
    val binding = org.json4s.native.JsonMethods.parse(result.binding)
    assert((binding \ "label_role") == org.json4s.JString("applied"))
    assert((binding \ "fame_owner_bit") == org.json4s.JInt(3))
    assert((binding \ "global_target_cut_proven") == org.json4s.JBool(false))
    assert((binding \ "deployable_capture_image") == org.json4s.JBool(false))
    def storage(s: CircuitState) = s.circuit.modules.collect { case m: Module =>
      val names = scala.collection.mutable.ArrayBuffer.empty[String]
      def visit(x: Statement): Unit = { x match { case r: DefRegister => names += r.serialize; case _ => () }; x.foreachStmt(visit) }
      visit(m.body); names.size
    }.sum
    assert(storage(state) == storage(result.state))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-cut"); Files.createDirectories(out)
      val e = new SemanticCleanEmitter
      val rtl = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
        result.state.copy(annotations = Seq(EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
      Seq("reference.fir" -> state.circuit.serialize, "reference.sv" -> emit(transform(false))._2,
        "wired.fir" -> result.state.circuit.serialize, "wired.sv" -> rtl,
        "binding.json" -> result.binding, "collector-binding.json" -> result.collector.binding,
        "manifest.json" -> inv, "plan.json" -> plan).foreach { case (n, body) =>
        Files.write(out.resolve(n), body.getBytes(UTF_8))
      }
    }
  }

  it should "compose native FAME observer and pending banks with the checked applied cut and staged collector" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val prepared = emit(transform(true))._1
    // The bank caller consumes the emitter request, as NativeFunctionalBankProbe
    // does. Keep every storage/constraint/progress annotation and prepared wire.
    val state = prepared.copy(form = LowForm, annotations = prepared.annotations.filterNot(_.isInstanceOf[EmitCircuitAnnotation]))
    val original = state.circuit.serialize
    val inv = SimulatorStateInventory.discover(state)
    val plan = PhysicalClockCutPlan.discover(state, inv)
    val result = FAMEClockCutWiring.generateNative(state, inv, plan, "hostReset", "clockA", 3)
    FAMEClockCutWiring.validateNative(state, inv, plan, "hostReset", "clockA", 3, result)
    val native = result.native.get
    val binding = parse(result.binding)
    val rows = (binding \ "signals").asInstanceOf[JArray].arr
    val stored = rows.filter(r => (r \ "storage") == JBool(true))
    assert(rows.size == 21 && stored.size == 8)
    val inventory = (parse(inv) \ "state").asInstanceOf[JArray].arr
    val banked = (parse(native.banks.binding) \ "state").asInstanceOf[JArray].arr
    stored.foreach { r =>
      val original = inventory.filter(e => (e \ "hierarchy") == (r \ "rtl_path"))
      val bank = banked.filter(e => (e \ "state_id") == (r \ "generated_state_id"))
      assert(original.size == 1 && bank.size == 1)
      assert((original.head \ "state_id") == (r \ "generated_state_id"))
      Seq("bank", "slot", "storage_hierarchy").foreach(k => assert((bank.head \ k) == (r \ k)))
    }
    assert(stored.count(r => (r \ "role") == JString("pending_flag")) == 2)
    assert(native.banks.entries.map(_.stateID).toSet == banked.map(e => (e \ "state_id").values.toString).toSet)
    val root = result.state.circuit.modules.collectFirst { case m: Module if m.name == "Top" => m }.get
    val text = root.body.serialize
    assert(root.ports.find(_.name == "checkpoint_f2_owner_present").get.tpe == UIntType(IntWidth(3)))
    assert(text.contains("cat(checkpoint_fame_cut_consistent, checkpoint_f2_owner_present)"))
    assert(text.contains("checkpoint_f2_adapter.target_cycle <= checkpoint_fame_cut_applied_cycle"))
    assert(text.contains("checkpoint_f2_shadow_adapter.target_cycle <= checkpoint_fame_cut_applied_cycle"))
    assert(text.contains("checkpoint_f2_shadow_permission <= and(checkpoint_f2_shadow_adapter.borrow_enable, checkpoint_f2_read_enable)"))
    assert(text.contains("ckpt_clock <= hostClock"))
    Seq("checkpoint_f2_bank_ckpt_req_valid", "ckpt_req_ready", "ckpt_resp_valid", "checkpoint_f2_bank_ckpt_resp_ready")
      .foreach(n => assert(text.linesIterator.find(_.trim.startsWith(n + " <=")).get.contains("checkpoint_f2_shadow_permission")))
    val gateModules = result.state.circuit.modules.collect { case e: ExtModule if e.defname == "BUFGCE" => e.name }.toSet
    def gateCount(s: Statement): Int = (s match {
      case i: WDefInstance if gateModules(i.module) => 1
      case _ => 0
    }) + { var count = 0; s.foreachStmt(x => count += gateCount(x)); count }
    assert(result.state.circuit.modules.collect { case m: Module => gateCount(m.body) }.sum == 3)
    Seq("global_target_cut_proven", "production_freeze_owners_bound", "complete_fpga_capture",
      "deployable_capture_image", "native_latch_restore_supported", "metasim_restore_supported")
      .foreach(k => assert((binding \ k) == JBool(false)))
    assert((binding \ "bank_binding_sha256") == JString(SemanticStateInventory.sha256(native.banks.binding)))
    assert((binding \ "staging_binding_sha256") == JString(SemanticStateInventory.sha256(native.staging.binding)))
    assert(state.circuit.serialize == original)
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-native-cut"); Files.createDirectories(out)
      val e = new SemanticCleanEmitter
      val rtl = new Compiler(Seq(firrtl.options.Dependency.fromTransform(e)), Forms.LowForm, Set(e)).execute(
        result.state.copy(annotations = Seq(EmitCircuitAnnotation(e.getClass)))).getEmittedCircuit.value
      val files = Seq("reference.fir" -> state.circuit.serialize, "reference.sv" -> emit(transform(false))._2,
        "wired.fir" -> result.state.circuit.serialize, "wired.sv" -> rtl,
        "binding.json" -> result.binding, "collector-binding.json" -> result.collector.binding,
        "staging-binding.json" -> native.staging.binding, "banks-binding.json" -> native.banks.binding,
        "banks.json" -> SemanticStateInventory.canonical(JObject(List("entries" -> JArray(native.banks.entries.map { e =>
          JObject(List("state_id" -> JString(e.stateID), "bank" -> JInt(e.bank), "slot" -> JInt(e.slot),
            "width" -> JInt(e.width), "depth" -> JInt(e.depth)))
        }.toList)))) ,
        "manifest.json" -> inv, "plan.json" -> plan, "dependencies.json" -> native.dependencies,
        "witness.fir" -> native.witness.circuit.serialize,
        "witness-manifest.json" -> native.manifest, "witness-plan.json" -> native.plan)
      files.foreach { case (n, body) => Files.write(out.resolve(n), body.getBytes(UTF_8)) }
      Files.write(out.resolve("emission.json"), SemanticStateInventory.canonical(JObject(List(
        "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
        "inputs" -> JObject(files.map { case (n, body) => n -> JString(SemanticStateInventory.sha256(body)) }.toList)
      ))).getBytes(UTF_8))
    }
  }

  it should "reject altered native FAME staging banks and coherently rehashed witness wiring" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val prepared = emit(transform(true))._1
    val state = prepared.copy(form = LowForm, annotations = prepared.annotations.filterNot(_.isInstanceOf[EmitCircuitAnnotation]))
    val inv = SimulatorStateInventory.discover(state); val plan = PhysicalClockCutPlan.discover(state, inv)
    val result = FAMEClockCutWiring.generateNative(state, inv, plan, "hostReset", "clockA", 3)
    val native = result.native.get
    val changed = Seq(result.copy(native = None),
      result.copy(collector = result.collector.copy(reset = result.collector.reset.copy(audit = "tampered"))),
      result.copy(native = Some(native.copy(dependencies = native.dependencies + " "))),
      result.copy(native = Some(native.copy(banks = native.banks.copy(entries = native.banks.entries.drop(1))))),
      result.copy(native = Some(native.copy(staging = native.staging.copy(binding = native.staging.binding + " ")))),
      result.copy(native = Some(native.copy(staging = native.staging.copy(collector = native.staging.collector.copy(
        reset = native.staging.collector.reset.copy(audit = "tampered")))))))
    changed.foreach(r => intercept[IllegalArgumentException] {
      FAMEClockCutWiring.validateNative(state, inv, plan, "hostReset", "clockA", 3, r)
    })
    for (sink <- Seq("checkpoint_f2_adapter.target_cycle", "checkpoint_f2_shadow_adapter.target_cycle",
        "checkpoint_f2_adapter.owner_present", "ckpt_resp_valid", "ckpt_clock")) {
      var count = 0
      def rewrite(s: Statement): Statement = s match {
        case c: Connect if c.loc.serialize == sink =>
          count += 1
          c.copy(expr = if (sink == "ckpt_clock") DoPrim(firrtl.PrimOps.AsClock, Seq(UIntLiteral(0, IntWidth(1))), Nil, ClockType)
            else UIntLiteral(0, IntWidth(if (sink.endsWith("target_cycle")) 64 else 1)))
        case x => x.mapStmt(rewrite)
      }
      val circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
        case m: Module if m.name == "Top" => m.copy(body = rewrite(m.body))
        case m => m
      })
      assert(count == 1)
      val mutated = circuit
      val binding = SemanticStateInventory.canonical(parse(result.binding).transformField {
        case ("wired_reference_sha256", _) => "wired_reference_sha256" -> JString(SemanticStateInventory.sha256(mutated.serialize))
      })
      intercept[IllegalArgumentException] {
        FAMEClockCutWiring.validateNative(state, inv, plan, "hostReset", "clockA", 3,
          result.copy(state = result.state.copy(circuit = mutated), binding = binding))
      }
    }
    intercept[IllegalArgumentException] {
      FAMEClockCutWiring.validate(state, inv, plan, "hostReset", "clockA", 3, result)
    }
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(state, inv, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
  }

  it should "reject incomplete roles domains stale inputs and coherently rehashed missing witness wiring" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val s = emit(transform(true))._1.copy(form = LowForm); val inv = SimulatorStateInventory.discover(s)
    val plan = PhysicalClockCutPlan.discover(s, inv)
    def generate(state: CircuitState = s, domain: String = "clockA", owners: Int = 3) =
      FAMEClockCutWiring.generate(state, inv, plan, "hostReset", domain, owners)
    for (domain <- Seq("", "missing")) intercept[IllegalArgumentException](generate(domain = domain))
    for (owners <- Seq(0, 65536)) intercept[IllegalArgumentException](generate(owners = owners))
    intercept[IllegalArgumentException](FAMEClockCutWiring.generate(s, inv + " ", plan, "hostReset", "clockA", 3))
    intercept[IllegalArgumentException](FAMEClockCutWiring.generate(s, inv, plan + " ", "hostReset", "clockA", 3))
    for (role <- Seq("applied", "pending_flag", "overflow", "gate_ce")) {
      val first = s.annotations.collectFirst { case a: FAMEClockProgressSignalAnnotation if a.role == role => a }.get
      intercept[IllegalArgumentException](generate(s.copy(annotations = s.annotations.filterNot(_ == first))))
    }
    val result = generate()
    val changedCircuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
      case m: Module if m.name == "Top" =>
        def change(x: Statement): Statement = x match {
          case c: Connect if c.loc.serialize == "checkpoint_f2_adapter.target_cycle" =>
            c.copy(expr = UIntLiteral(0, IntWidth(64)))
          case other => other.mapStmt(change)
        }
        m.copy(body = change(m.body))
      case m => m
    })
    val changedBinding = SemanticStateInventory.canonical(parse(result.binding).transformField {
      case ("wired_reference_sha256", _) => "wired_reference_sha256" -> JString(SemanticStateInventory.sha256(changedCircuit.serialize))
    })
    intercept[IllegalArgumentException](FAMEClockCutWiring.validate(s, inv, plan, "hostReset", "clockA", 3,
      result.copy(state = result.state.copy(circuit = changedCircuit), binding = changedBinding)))
    intercept[IllegalArgumentException](FAMEClockCutWiring.validate(s, inv, plan, "hostReset", "clockB", 3, result))
  }

  it should "require an isolated FAME collector request and emit source-bound siblings only" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val s = emit(transform(true))._1.copy(form = LowForm)
    val as = Seq(SimulatorCheckpointInventoryAnnotation("Top"), PhysicalClockCutPlanAnnotation,
      PhysicalClockCutWiringProbeAnnotation, SynchronousResetCutWiringAnnotation("hostReset"),
      FAMEClockCutWiringAnnotation("clockA", 3))
    val input = s.copy(annotations = s.annotations ++ as)
    PhysicalClockCutPlan.validateCardinality(input.annotations)
    for (bad <- Seq(as :+ as.last, as :+ F2ClockResetCutWiringAnnotation("bad", 3), as.dropRight(2) :+ as.last,
        as.dropRight(1) :+ FAMEClockCutWiringAnnotation("bad:domain", 3)))
      intercept[IllegalArgumentException](FAMEClockCutWiring.validateRequest(bad))
    val inv = SimulatorStateInventory.discover(input); val plan = PhysicalClockCutPlan.discover(input, inv)
    val files = FAMEClockCutWiring.emit(input, inv, plan)
    assert(files.size == 5 && files.forall(_.fileSuffix.startsWith(".f2-fame-clock-cut")))
    val binding = parse(files.find(_.fileSuffix.endsWith("-binding.json")).get.body)
    assert((binding \ "deployable_capture_image") == JBool(false))
    intercept[IllegalArgumentException](FAMEClockCutWiring.emit(input.copy(annotations = input.annotations ++ files), inv, plan))
    assert(FAMEClockCutWiring.emit(s, inv, plan).isEmpty)
  }

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
    assert(FAMEClockProgressBindings.resolve(observed).size == 21)
    val (actual, rtl) = emit(observed); val (_, originalRTL) = emit(reference)
    val inventory = SimulatorStateInventory.discover(actual)
    assert(inventory.contains("checkpoint_fame_clockA_applied"))
    assert(inventory.contains("checkpoint_fame_clockB_consumed"))
    FAMEClockProgressBindings.validatePreserved(observed, actual)
    val binding = FAMEClockProgressBindings.emit(actual, rtl, inventory).head.body
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-progress"); Files.createDirectories(out)
      Seq("reference.fir" -> reference.circuit.serialize, "observed.fir" -> observed.circuit.serialize,
        "observed.sv" -> rtl, "reference.sv" -> originalRTL, "probed.sv" -> emit(probes(observed))._2,
        "state.json" -> inventory, "progress-binding.json" -> binding, "emission-reference.fir" -> actual.circuit.serialize,
        "progress-annotations.json" -> JsonProtocol.serialize(actual.annotations.collect {
          case a: FAMEClockProgressSignalAnnotation => a
          case a: FAMEClockProgressModelAnnotation => a
        })).foreach {
        case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8))
      }
    }
  }
  private def bankReference(): CircuitState = {
    val s = transform(true)
    new Compiler(Forms.LowForm).execute(s.copy(form = HighForm, annotations = s.annotations.filter {
      case _: DontTouchAnnotation | _: FAMEClockProgressSignalAnnotation | _: FAMEClockProgressModelAnnotation => true
      case _ => false
    })).copy(form = LowForm)
  }

  it should "specialize complete FAME identities into native functional banks and hash every translated role" in {
    val reference = bankReference()
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val result = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    GeneratedStateBankAccess.validateAnnotationBinding(result)
    FAMEClockProgressBindings.validatePreserved(reference, result.state)
    val original = reference.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    val translated = result.state.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    assert(translated.size == 21 && translated.map(_.target).distinct.size == 21)
    assert(translated.map(_.model).toSet == original.map(_.model).toSet)
    assert(translated.forall(_.target.module == "Top"))
    assert(translated.forall(_.target.path.head._1 == original.head.target.path.head._1))
    assert(translated.forall(_.target.path.head._2.value != "Model"))
    val gates = translated.filter(a => Set("gate_ce", "gate_output")(a.role))
    assert(gates.size == 4 && gates.forall(a => a.target.path.size == 2 && a.target.component.isEmpty))
    assert(result.state.circuit.modules.collect { case m: ExtModule => m } ==
      reference.circuit.modules.collect { case m: ExtModule => m })
    val descriptors = midas.widgets.CheckpointCaptureDescriptors.fromNativeGenerated(result, reference, manifest, plan, deps)
    assert(descriptors.entries.nonEmpty)
    // Every loss/retarget/identity mutation invalidates the actual bank binding,
    // even though all ordinary storage and its original manifest are unchanged.
    translated.foreach { role =>
      Seq(result.state.annotations.filterNot(_ == role), result.state.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == role => a.copy(model = a.model + "_changed")
        case a => a
      }, result.state.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == role => a.copy(target = a.target.copy(ref = "missing"))
        case a => a
      }).foreach { changed =>
        intercept[IllegalArgumentException] {
          GeneratedStateBankAccess.validateAnnotationBinding(result.copy(state = result.state.copy(annotations = changed)))
        }
      }
    }
    val model = result.state.annotations.collectFirst { case a: FAMEClockProgressModelAnnotation => a }.get
    intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.validateAnnotationBinding(result.copy(state = result.state.copy(
        annotations = result.state.annotations.filterNot(_ == model))))
    }
    // The ordinary hardware owner guard must now be reached rather than hiding
    // behind an unsupported-annotation diagnostic. No physical CE is fabricated.
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(reference, manifest, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-progress-banks"); Files.createDirectories(out)
      Seq("reference.fir" -> reference.circuit.serialize, "manifest.json" -> manifest,
        "plan.json" -> plan, "dependencies.json" -> deps, "instrumented.fir" -> result.state.circuit.serialize,
        "instrumented.sv" -> emit(result.state)._2, "binding.json" -> result.binding,
        "annotations.json" -> JsonProtocol.serialize(result.state.annotations),
        "descriptors.json" -> descriptors.manifest).foreach {
        case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8))
      }
    }
  }

  it should "reject incomplete replicated aliased and stale FAME roles before bank specialization" in {
    val reference = bankReference()
    val roles = reference.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    val first = roles.head
    val bad = Seq(reference.annotations.filterNot(_.isInstanceOf[FAMEClockProgressModelAnnotation]),
      reference.annotations.filterNot(_ == first), reference.annotations :+ first,
      reference.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == first => a.copy(target = roles(1).target)
        case a => a
      }, reference.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == first => a.copy(target = a.target.copy(circuit = "Foreign"))
        case a => a
      }, reference.annotations.map {
        case a: FAMEClockProgressModelAnnotation => a.copy(domains = a.domains :+ "missing")
        case a => a
      })
    bad.foreach { annotations =>
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAnnotations.preflight(reference.copy(annotations = annotations))
      }.getMessage.contains("FAME clock progress binding unsupported"))
    }
  }

  it should "bind native bank roles and original StateIDs through the companion's actual final emitter" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val reference = bankReference()
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(banks.state.copy(annotations = banks.state.annotations :+ EmitCircuitAnnotation(emitter.getClass)))
    val actual = emitter.preparedState.get
    val rtl = emitted.getEmittedCircuit.value
    val files = FAMEClockProgressBindings.emitNativeBanked(banks, actual, rtl)
    assert(files.size == 1)
    val binding = parse(files.head.body)
    assert((binding \ "emitted_verilog_sha256") == JString(SemanticStateInventory.sha256(rtl)))
    assert((binding \ "emission_reference_sha256") == JString(SemanticStateInventory.sha256(actual.circuit.serialize)))
    assert((binding \ "bank_binding_sha256") == JString(SemanticStateInventory.sha256(banks.binding)))
    val roles = (binding \ "signals").asInstanceOf[JArray].arr
    assert(roles.size == 21 && roles.filter(r => (r \ "storage") == JBool(true)).size == 8)
    val inventory = (parse(banks.binding) \ "state").asInstanceOf[JArray].arr
    roles.filter(r => (r \ "storage") == JBool(true)).foreach { role =>
      val entry = inventory.find(e => (e \ "state_id") == (role \ "generated_state_id")).get
      assert((role \ "bank") == (entry \ "bank") && (role \ "slot") == (entry \ "slot"))
      assert((role \ "input_storage_hierarchy") == (entry \ "storage_hierarchy"))
    }
    val requested = reference.copy(annotations = reference.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(reference.circuit.main),
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)))
    val companion = NativeFunctionalBankProbe(requested)
    assert(companion.circuit == reference.circuit)
    val outputs = companion.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }
    val receipt = parse(outputs.find(_.fileSuffix == ".native-functional-emission.json").get.body)
    Seq(".native-functional-fame-clock-progress.json", ".native-functional-emitted.fir",
      ".native-functional-emitted-progress.anno.json").foreach { name =>
      val file = outputs.find(_.fileSuffix == name).get
      assert((receipt \ "inputs" \ name) == JString(SemanticStateInventory.sha256(file.body)))
    }
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-progress-companion"); Files.createDirectories(out)
      outputs.foreach(a => Files.write(out.resolve("Top" + a.fileSuffix), a.body.getBytes(UTF_8)))
    }
  }

  it should "reject role loss and retargeting at the native bank emitter boundary" in {
    val reference = bankReference()
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(banks.state.copy(annotations = banks.state.annotations :+ EmitCircuitAnnotation(emitter.getClass)))
    val actual = emitter.preparedState.get
    val roles = actual.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    roles.foreach { role =>
      Seq(actual.annotations.filterNot(_ == role), actual.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == role => a.copy(target = a.target.copy(ref = "missing"))
        case a => a
      }).foreach { annotations =>
        intercept[IllegalArgumentException] {
          FAMEClockProgressBindings.emitNativeBanked(banks, actual.copy(annotations = annotations), emitted.getEmittedCircuit.value)
        }
      }
    }
    intercept[IllegalArgumentException] {
      FAMEClockProgressBindings.emitNativeBanked(banks, actual.copy(annotations = actual.annotations.filterNot {
        case _: FAMEClockProgressModelAnnotation | _: FAMEClockProgressSignalAnnotation => true
        case _ => false
      }), emitted.getEmittedCircuit.value)
    }
    val wired = PhysicalClockCutWiring.generateNative(reference, manifest, plan, deps, banks)
    val placement = midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)
    val missing = wired.state.annotations.filterNot(_.isInstanceOf[FAMEClockProgressSignalAnnotation])
    val changed = wired.state.copy(circuit = wired.state.circuit.copy(info = FileInfo("changed")))
    Seq(wired.copy(state = wired.state.copy(annotations = missing)), wired.copy(state = changed),
      wired.copy(binding = wired.binding + " ")).foreach { bad =>
      assert(intercept[IllegalArgumentException] {
        NativeCheckpointConstraints.emitNative(reference, manifest, plan, deps, banks, bad,
          reference.circuit.main + "CheckpointStateAccess", placement)
      }.getMessage.contains("changed clock-mask circuit or annotations"))
    }
    // The ordinary constraint path still rejects masked bank roles. Only the
    // same-reference, independently reconstructed native path may emit them.
    intercept[IllegalArgumentException] {
      NativeCheckpointConstraints.emit(wired.state, reference.circuit.main + "CheckpointStateAccess", placement)
    }
  }

  it should "reject changed bank freeze condition hold value and inner reset ownership" in {
    val reference = bankReference()
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val result = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    val role = result.state.annotations.collectFirst {
      case a: FAMEClockProgressSignalAnnotation if a.role == "completed_steps" => a
    }.get
    val owner = role.target.path.last._2.value
    for (mutation <- 0 until 3) {
      var changed = 0
      def rewrite(s: Statement): Statement = s match {
        case c: Connect if c.loc.serialize == role.target.ref =>
          changed += 1
          val m = c.expr.asInstanceOf[Mux]
          c.copy(expr = mutation match {
            case 0 => m.copy(cond = UIntLiteral(0, IntWidth(1)))
            case 1 => m.copy(tval = UIntLiteral(0, IntWidth(64)))
            case 2 => m.copy(fval = WRef(role.target.ref, UIntType(IntWidth(64))))
          })
        case s => s.mapStmt(rewrite)
      }
      val circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
        case m: Module if m.name == owner => m.copy(body = rewrite(m.body))
        case m => m
      })
      assert(changed == 1)
      val error = intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.validateAnnotationBinding(result.copy(state = result.state.copy(circuit = circuit)))
      }
      assert(error.getMessage.contains("FAME clock progress binding unsupported"))
    }
    // Ordinary production validation never silently accepts banked semantics.
    assert(intercept[IllegalArgumentException] {
      FAMEClockProgressBindings.resolve(result.state)
    }.getMessage.contains("reset owner mismatch"))
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
    val domain = FAMEClockProgress.Domain("clockA", bit, bit, bit, "gate")
    def instrument(module: Module = m, ds: Seq[FAMEClockProgress.Domain] = Seq(domain)) =
      FAMEClockProgress.instrument("Top", module, clock, bit, bit, ds)
    for (ds <- Seq(Nil, Seq(domain, domain), Seq(domain.copy(incoming = clock)), Seq(domain.copy(name = "a.b"))))
      intercept[IllegalArgumentException](instrument(ds = ds))
    intercept[IllegalArgumentException](instrument(m.copy(ports = m.ports :+ Port(NoInfo, "checkpoint_fame_external", Input, bit.tpe))))
    intercept[IllegalArgumentException](instrument(m.copy(body = Block(Seq(m.body, DefWire(NoInfo, "checkpoint_fame_hidden", bit.tpe))))))
  }
  it should "follow actual native inlining and emitter renames without guessing role names" in {
    val original = transform(true)
    val low = new Compiler(Forms.LowForm).execute(original.copy(form = HighForm))
    val inline = new Compiler(Seq(firrtl.options.Dependency[firrtl.passes.InlineInstances]), Forms.LowForm).execute(
      low.copy(annotations = low.annotations :+
        firrtl.passes.InlineAnnotation(ModuleName("Model", CircuitName("Top")))))
    FAMEClockProgressBindings.validatePreserved(original, inline)
    val (actual, rtl) = emit(inline)
    FAMEClockProgressBindings.validatePreserved(original, actual)
    val rows = FAMEClockProgressBindings.resolve(actual)
    assert(rows.size == 21)
    assert(actual.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a.target.module }.toSet == Set("Top"))
    val binding = FAMEClockProgressBindings.emit(actual, rtl, SimulatorStateInventory.discover(actual)).head.body
    assert(binding.contains("Top.model__checkpoint_fame_clockA_applied"))
    val roundtrip = JsonProtocol.deserialize(JsonProtocol.serialize(actual.annotations.collect {
      case a: FAMEClockProgressSignalAnnotation => a
      case a: FAMEClockProgressModelAnnotation => a
    }))
    assert(roundtrip.size == 22)
    assert(FAMEClockProgressBindings.resolve(actual.copy(annotations = roundtrip)) == rows)
  }
  it should "reject missing duplicate unknown and aliased roles before final emission" in {
    val state = transform(true)
    val signals = state.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    val first = signals.head
    val bad = Seq(
      state.annotations.filterNot(_ == first), state.annotations :+ first,
      state.annotations.filterNot(_.isInstanceOf[FAMEClockProgressModelAnnotation]),
      state.annotations.map { case a: FAMEClockProgressSignalAnnotation if a == first => a.copy(role = "unknown"); case a => a },
      state.annotations.map { case a: FAMEClockProgressSignalAnnotation if a == first => a.copy(target = signals(1).target); case a => a })
    bad.foreach(as => intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(state.copy(annotations = as))))
    bad.take(4).foreach(as => intercept[IllegalArgumentException](FAMEClockProgressBindings.validatePreserved(state, state.copy(annotations = as))))
    val wrongModel = state.annotations.map {
      case a: FAMEClockProgressModelAnnotation => a.copy(domains = a.domains :+ "fakeClock")
      case a => a
    }
    intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(state.copy(annotations = wrongModel)))
  }
  it should "bind the actual production emitter and reject host passes that erase the entire role set" in {
    val observed = transform(true)
    val low = new Compiler(Forms.LowForm).execute(observed.copy(form = HighForm))
    val output = midas.stage.FAMEClockProgressPipelineFixture.compile(low)
    val binding = output.collectFirst {
      case midas.stage.GoldenGateOutputFileAnnotation(body, ".fame-clock-progress.json") => body
    }.get
    val rtl = output.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
    assert(binding.contains(SemanticStateInventory.sha256(rtl)))
    assert(binding.contains("\"global_target_cut_proven\":false"))
    val error = intercept[IllegalArgumentException](midas.stage.FAMEClockProgressPipelineFixture.eraseRoles(low))
    assert(error.getMessage.contains("construction-site identities lost or duplicated"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = Paths.get(base).resolve("fame-progress"); Files.createDirectories(out)
      Files.write(out.resolve("production-binding.json"), binding.getBytes(UTF_8))
      Files.write(out.resolve("production.sv"), rtl.getBytes(UTF_8))
    }
  }
  it should "reject coherent gate swaps and pending or register clock corruption" in {
    val state = transform(true)
    val signals = state.annotations.collect { case a: FAMEClockProgressSignalAnnotation => a }
    def change(role: String): CircuitState = state.copy(annotations = state.annotations.map {
      case a: FAMEClockProgressSignalAnnotation if a.domain == "clockA" && a.role == role =>
        a.copy(target = signals.find(b => b.domain == "clockB" && b.role == role).get.target)
      case a => a
    })
    for (role <- Seq("gate_ce", "gate_output", "pending_flag", "applied"))
      intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(change(role)))
    val swapped = state.copy(annotations = state.annotations.map {
      case a: FAMEClockProgressSignalAnnotation if a.domain.nonEmpty && Set("gate_ce", "gate_output")(a.role) =>
        a.copy(target = signals.find(b => b.domain != a.domain && b.role == a.role).get.target)
      case a => a
    })
    intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(swapped))
    def corruptStatement(s: Statement): Statement = s match {
      case r: DefRegister if r.name == "checkpoint_fame_clockA_applied" => r.copy(clock = firrtl.WRef("clockA_enabled", ClockType))
      case x => x.mapStmt(corruptStatement)
    }
    val corrupt = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module if m.name == "Model" => m.copy(body = corruptStatement(m.body))
      case m => m
    }))
    intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(corrupt))
  }
  it should "reject foreign missing reshaped and replicated execution targets" in {
    val state = transform(true)
    val first = state.annotations.collectFirst { case a: FAMEClockProgressSignalAnnotation => a }.get
    for (target <- Seq(first.target.copy(circuit = "Other"), first.target.copy(ref = "missing"),
        first.target.field("unknown"), first.target.copy(module = "missing"))) {
      val bad = state.copy(annotations = state.annotations.map {
        case a: FAMEClockProgressSignalAnnotation if a == first => a.copy(target = target)
        case a => a
      })
      intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(bad))
    }
    val replicated = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module if m.name == "Top" => m.copy(body = Block(Seq(m.body, WDefInstance("replica", "Model"))))
      case m => m
    }))
    intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(replicated))
    val gate = state.annotations.collectFirst {
      case a: FAMEClockProgressSignalAnnotation if a.role == "gate_ce" => a
    }.get
    val forged = gate.copy(target = gate.target.copy(path = gate.target.path.updated(gate.target.path.size - 1,
      gate.target.path.last._1 -> TargetToken.OfModule("UnrelatedGate"))))
    intercept[IllegalArgumentException](FAMEClockProgressBindings.resolve(state.copy(annotations =
      state.annotations.map { case a if a == gate => forged; case a => a })))
  }
}
