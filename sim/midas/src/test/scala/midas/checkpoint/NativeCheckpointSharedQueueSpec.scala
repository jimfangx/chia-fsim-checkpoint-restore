// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import midas.targetutils.xdc.{RAMStyleHint, RAMStyles, XDCAnnotation, XDCFiles}

class NativeCheckpointSharedQueueSpec extends AnyFlatSpec {
  private def source: CircuitState = {
    val resource = scala.io.Source.fromResource("checkpoint/SharedQueue.firrtl")
    val text = try resource.mkString finally resource.close()
    // This fixture is already ground, explicit LowForm; resolving directly
    // preserves the exact shared definition and concrete execution names.
    val working = firrtl.passes.ToWorkingIR.run(Parser.parse(text))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))
  }
  private val top = ModuleTarget("SharedQueueHost", "SharedQueueHost")
  private val selected = Seq("q0", "q1").map(top.instOf(_, "SharedQueue"))
  private def emit(state: CircuitState): String = {
    val emitter = new SemanticCleanEmitter
    val constrained = midas.passes.WriteXDCFile.execute(state.copy(annotations = state.annotations :+
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))))
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Concrete shared queue local-state ownership")
  it should "specialize selected executions with independent banks and preserve the active original sibling" in {
    val h = source
    val hint = XDCAnnotation(XDCFiles.Synthesis, RAMStyleHint.propertyTemplate(RAMStyles.BLOCK),
      ModuleTarget(h.circuit.main, "SharedQueue").ref("ram"))
    val annotated = h.copy(annotations = h.annotations :+ hint)
    val result = NativeCheckpointRegions.generate(annotated, Seq.empty, shellTargets = selected)
    NativeCheckpointRegions.validate(result)
    result.shells.foreach { shell =>
      val local = shell.banks.state.circuit.modules.head.asInstanceOf[Module]
      val memory = local.body.serialize
      assert(memory.contains("read-latency => 1") && !memory.contains("reader => ckpt_read"))
      assert(memory.contains("reg CheckpointPipeline_ram_r_addr"))
      assert(memory.contains("reg CheckpointNativeHold_ram"))
      assert(memory.contains("read-under-write => old"))
      assert(memory.contains("reg CheckpointNativeForwardHit_ram : UInt<1>, clock"))
      assert(memory.contains("reg CheckpointNativeForwardData_ram : UInt<130>, clock"))
      assert((parse(shell.banks.binding) \ "synchronous_memory_readout" \ "adapter_version") == JInt(3))
      assert((parse(shell.banks.binding) \ "synchronous_memory_readout" \ "write_first_forwarding") ==
        JString("registered_hit_and_data_at_original_ram_edge"))
      assert((parse(shell.banks.binding) \ "synchronous_memory_readout" \ "response_latency_edges") == JInt(4))
      assert((parse(shell.banks.binding) \ "synchronous_memory_readout" \ "registered_request_before_borrow") == JBool(true))
      assert((parse(shell.banks.binding) \ "synchronous_memory_readout" \ "original_reader_reprimed_before_response") == JBool(true))
      val observation = parse(shell.banks.binding) \ "native_state_observation"
      assert((observation \ "bits") == JInt(518) && (observation \ "words") == JInt(9))
      val fields = (observation \ "state").asInstanceOf[JArray].arr
      assert(fields.size == 14 && fields.exists(r => (r \ "expression") == JString("ram.r.data") &&
        (r \ "width") == JInt(130) && (r \ "adapter") == JString("firrtl-emitter-memory-pipeline-v1")))
      assert((observation \ "complete_capture_owner") == JBool(false) &&
        (observation \ "atomic_snapshot_supported") == JBool(false) &&
        (observation \ "stable_while_functional_freeze") == JBool(false))
      assert(local.ports.exists(p => p.name == NativeSynchronousStateObservation.port &&
        p.direction == Output && p.tpe == UIntType(IntWidth(518))))
      assert(shell.entries.size == 5)
    }
    val before = GeneratedExternalOwners.instances(annotated, h.circuit.main).toMap
    val after = GeneratedExternalOwners.instances(result.state, h.circuit.main).toMap
    assert(before.keySet == after.keySet)
    assert(after("SharedQueueHost.q2") == before("SharedQueueHost.q2"))
    assert(after("SharedQueueHost.q0").name == "checkpoint_shell_region0_selected_SharedQueue")
    assert(after("SharedQueueHost.q1").name == "checkpoint_shell_region1_selected_SharedQueue")
    assert(result.state.circuit.modules.find(_.name == "SharedQueue") ==
      annotated.circuit.modules.find(_.name == "SharedQueue"))
    assert(result.descriptors.entries.size == 10 && result.descriptors.physicalClockGates == 0)
    val manifest = parse(result.descriptors.manifest)
    assert((manifest \ "uncovered_functional_state").asInstanceOf[JArray].arr.size == 5)
    assert((manifest \ "uncovered_physical_gates").asInstanceOf[JArray].arr.size == 1)
    val rows = (manifest \ "state").asInstanceOf[JArray].arr
    assert(rows.forall(r => (r \ "hierarchy").asInstanceOf[JString].s.startsWith("SharedQueueHost.q")))
    assert(result.descriptors.entries.map(_.stateID).distinct.size == 10)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("shared-queue-access")
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    save("source.fir", annotated.circuit.serialize); save("generated.fir", result.state.circuit.serialize)
    save("source.sv", emit(annotated)); save("generated.sv", emit(result.state))
    save("source.anno.json", JsonProtocol.serialize(annotated.annotations))
    save("generated.anno.json", JsonProtocol.serialize(result.state.annotations))
    save("binding.json", result.binding); save("descriptors.json", result.descriptors.manifest)
    save("descriptor-binding.json", result.descriptors.binding); save("routing.json", result.routing.binding)
    val graft = NativeCheckpointRegionGraft.generate(result)
    val observation = parse(graft.binding) \ "native_state_observation"
    assert((observation \ "bits") == JInt(1036))
    val native = (observation \ "regions").asInstanceOf[JArray].arr
    assert(native.map(_ \ "offset") == Seq(JInt(0), JInt(518)))
    assert(native.map(_ \ "selected_execution") == Seq(JString("SharedQueueHost.q0"), JString("SharedQueueHost.q1")))
    assert((observation \ "atomic_snapshot_supported") == JBool(false))
    assert((observation \ "stable_while_functional_freeze") == JBool(false))
    assert(NativeCheckpointInPlaceGraft.capturePorts(0).forall {
      case Port(_, _, _, UIntType(IntWidth(w))) => w > 0
      case _ => false
    })
    intercept[IllegalArgumentException](NativeCheckpointInPlaceGraft.capturePorts(-1))
    intercept[IllegalArgumentException](NativeCheckpointInPlaceGraft.capturePorts(65536))
    save("grafted.fir", graft.state.circuit.serialize); save("grafted.sv", emit(graft.state))
    graft.files.foreach(f => save("FireSim-generated" + f.fileSuffix, f.body))
    Seq("source" -> annotated, "generated" -> result.state).foreach { case (name, state) =>
      val constrained = midas.passes.WriteXDCFile.execute(state.copy(annotations = state.annotations :+
        midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))))
      constrained.annotations.collect { case a: midas.passes.XDCOutputAnnotation => a }.foreach { a =>
        save(name + a.suffix.get, a.fileBody)
      }
    }
    result.shells.zipWithIndex.foreach { case (shell, index) =>
      save(s"shell-$index-mapping.json", shell.mapping)
      save(s"shell-$index-binding.json", shell.binding)
      save(s"shell-$index-manifest.json", shell.manifest)
      save(s"shell-$index-banks-binding.json", shell.banks.binding)
      save(s"shell-$index-reference.fir", shell.reference.circuit.serialize)
      save(s"shell-$index-banked.fir", shell.banks.state.circuit.serialize)
    }
    val damaged = result.state.copy(circuit = result.state.circuit.copy(info = FileInfo("changed")))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(result.copy(state = damaged)))
    println(s"SHARED_QUEUE_ACCESS out=$out selected=2 untouched=1 descriptors=10")
  }

  it should "reject unsupported synchronous native shapes, disabled readers and foreign clocks" in {
    val local = NativeCheckpointShellStateAccess.generate(source, selected.head).reference
    val original = local.circuit.modules.head.asInstanceOf[Module]
    def attempt(change: Statement => Statement, extraPorts: Seq[Port] = Nil): Unit = {
      def rewrite(s: Statement): Statement = change(s.mapStmt(rewrite))
      val changed = local.copy(circuit = local.circuit.copy(modules = Seq(original.copy(
        ports = original.ports ++ extraPorts, body = rewrite(original.body)))))
      intercept[IllegalArgumentException] {
        val banks = GeneratedStateBankAccess.generate(changed, SimulatorStateInventory.discover(changed),
          separateAccessClock = true)
        NativeSynchronousMemoryReadout.generate(changed, banks)
      }
    }
    attempt { case m: DefMemory => m.copy(readUnderWrite = ReadUnderWrite.Old); case x => x }
    attempt { case m: DefMemory => m.copy(readLatency = 0); case x => x }
    attempt { case m: DefMemory => m.copy(readers = m.readers :+ "other"); case x => x }
    attempt { case c: Connect if c.loc.serialize == "ram.r.en" =>
      c.copy(expr = UIntLiteral(0, IntWidth(1))); case x => x }
    attempt({ case c: Connect if c.loc.serialize == "ram.w.clk" =>
      c.copy(expr = firrtl.WRef("other_clock", ClockType)); case x => x },
      Seq(Port(NoInfo, "other_clock", Input, ClockType)))
  }

  it should "preserve direct reader feedback and reject changed adapter policy or access clocks" in {
    val local = NativeCheckpointShellStateAccess.generate(source, selected.head).reference
    val original = local.circuit.modules.head.asInstanceOf[Module]
    def memories(s: Statement): Seq[DefMemory] = s match {
      case m: DefMemory => Seq(m)
      case x => { val out = scala.collection.mutable.ArrayBuffer.empty[DefMemory]
        x.foreachStmt(s => out ++= memories(s)); out.toSeq }
    }
    val ram = memories(original.body).head
    def feedback(s: Statement): Statement = s match {
      case c: Connect if c.loc.serialize == "ram.r.addr" =>
        c.copy(expr = DoPrim(firrtl.PrimOps.Bits,
          Seq(firrtl.passes.MemPortUtils.memPortField(ram, "r", "data")), Seq(BigInt(2), BigInt(0)), UIntType(IntWidth(3))))
      case x => x.mapStmt(feedback)
    }
    val changed = local.copy(circuit = local.circuit.copy(modules = Seq(original.copy(body = feedback(original.body)))))
    val ordinary = GeneratedStateBankAccess.generate(changed, SimulatorStateInventory.discover(changed),
      separateAccessClock = true)
    val adapted = NativeSynchronousMemoryReadout.generate(changed, ordinary)
    val generated = adapted.state.circuit.modules.head.asInstanceOf[Module]
    def connections(s: Statement): Seq[Connect] = s match {
      case c: Connect => Seq(c)
      case x => { val out = scala.collection.mutable.ArrayBuffer.empty[Connect]
        x.foreachStmt(s => out ++= connections(s)); out.toSeq }
    }
    assert(connections(generated.body).find(_.loc.serialize == "ram.r.addr").get.expr.serialize
      .contains("CheckpointNativeBorrowed"))
    val policy = SemanticStateInventory.canonical(parse(adapted.binding).transformField {
      case ("response_latency_edges", _) => "response_latency_edges" -> JInt(1)
    })
    intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(adapted.copy(binding = policy)))
    def wrongClock(s: Statement): Statement = s match {
      case r: DefRegister if r.name == "ckpt_native_phase" => r.copy(clock = firrtl.WRef("clock", ClockType))
      case x => x.mapStmt(wrongClock)
    }
    val state = adapted.state.copy(circuit = adapted.state.circuit.copy(modules = Seq(
      generated.copy(body = wrongClock(generated.body)))))
    val rehashed = SemanticStateInventory.canonical(parse(adapted.binding).transformField {
      case ("instrumented_sha256", _) => "instrumented_sha256" -> JString(SemanticStateInventory.sha256(state.circuit.serialize))
    })
    intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(
      adapted.copy(state = state, binding = rehashed)))
  }

  it should "reject coherently rehashed native equations and require an immutable original bank" in {
    val local = NativeCheckpointShellStateAccess.generate(source, selected.head).reference
    val ordinary = GeneratedStateBankAccess.generate(local, SimulatorStateInventory.discover(local),
      separateAccessClock = true)
    val adapted = NativeSynchronousMemoryReadout.generate(local, ordinary)
    GeneratedStateBankAccess.validateAnnotationBinding(adapted)
    assert(adapted.synchronousOrigin.nonEmpty && ordinary.synchronousOrigin.isEmpty)
    val lost = intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(
      adapted.copy(synchronousOrigin = None)))
    assert(lost.getMessage.contains("missing original source/bank provenance"))
    val root = adapted.state.circuit.modules.head.asInstanceOf[Module]
    val cases: Seq[(String, Statement => Statement)] = Seq(
      "forward_hit" -> { case c: Connect if c.loc.serialize == "CheckpointNativeForwardHit_ram" =>
        c.copy(expr = UIntLiteral(0, IntWidth(1))); case s => s },
      "forward_data" -> { case c: Connect if c.loc.serialize == "CheckpointNativeForwardData_ram" =>
        c.copy(expr = UIntLiteral(0, IntWidth(130))); case s => s },
      "public_shadow" -> { case c: Connect if c.loc.serialize == "CheckpointNativeHold_ram" =>
        c.copy(expr = UIntLiteral(0, IntWidth(130))); case s => s },
      "borrowed" -> { case c: Connect if c.loc.serialize == "CheckpointNativeBorrowed" =>
        c.copy(expr = UIntLiteral(0, IntWidth(1))); case s => s },
      "accepting_edge_borrow" -> { case n: DefNode if n.name == "ckpt_native_borrow" =>
        n.copy(value = firrtl.WRef("ckpt_accept", UIntType(IntWidth(1)))); case s => s },
      "transient_hold_loss" -> { case c: Connect if c.loc.serialize == "ckpt_native_active" =>
        c.copy(expr = firrtl.WRef("ckpt_native_active", UIntType(IntWidth(1)))); case s => s },
      "native_address" -> { case c: Connect if c.loc.serialize == "ram.r.addr" =>
        c.copy(expr = UIntLiteral(0, IntWidth(3))); case s => s },
      "native_enable" -> { case c: Connect if c.loc.serialize == "ram.r.en" =>
        c.copy(expr = UIntLiteral(0, IntWidth(1))); case s => s },
      "endpoint_phase" -> { case c: Connect if c.loc.serialize == "ckpt_native_phase" =>
        c.copy(expr = UIntLiteral(0, IntWidth(2))); case s => s },
      "response_data" -> { case c: Connect if c.loc.serialize == "ckpt_saved_data" =>
        c.copy(expr = UIntLiteral(0, IntWidth(64))); case s => s },
      "response_error" -> { case c: Connect if c.loc.serialize == "ckpt_saved_error" =>
        c.copy(expr = UIntLiteral(0, IntWidth(1))); case s => s },
      "reset_cancel" -> { case c: Connect if c.loc.serialize == "ckpt_native_request" =>
        c.copy(expr = UIntLiteral(1, IntWidth(1))); case s => s },
      "observation_vector" -> { case c: Connect if c.loc.serialize == NativeSynchronousStateObservation.port =>
        c.copy(expr = UIntLiteral(0, IntWidth(518))); case s => s })
    cases.foreach { case (name, change) =>
      def rewrite(s: Statement): Statement = change(s.mapStmt(rewrite))
      val state = adapted.state.copy(circuit = adapted.state.circuit.copy(modules = Seq(
        root.copy(body = rewrite(root.body)))))
      assert(state.circuit != adapted.state.circuit, name)
      val rehashed = SemanticStateInventory.canonical(parse(adapted.binding).transformField {
        case ("instrumented_sha256", _) => "instrumented_sha256" ->
          JString(SemanticStateInventory.sha256(state.circuit.serialize))
      })
      val error = intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(
        adapted.copy(state = state, binding = rehashed)))
      assert(error.getMessage.contains("changed source-bound native RAM"), name + ": " + error.getMessage)
    }
    val wrongEntry = adapted.entries.head.copy(depth = adapted.entries.head.depth + 1)
    intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(
      adapted.copy(entries = wrongEntry +: adapted.entries.tail)))
    val wrongOrdinary = ordinary.copy(entries = ordinary.entries.tail)
    assert(intercept[IllegalArgumentException](NativeSynchronousMemoryReadout.generate(local, wrongOrdinary))
      .getMessage.contains("ordinary bank does not match the original reference"))
    // Removing both public metadata markers must not disable validation while
    // the original proof remains attached to a copied result.
    val hiddenPolicy = SemanticStateInventory.canonical(JObject(parse(adapted.binding).asInstanceOf[JObject].obj
      .filterNot(_._1 == "synchronous_memory_readout")))
    intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(adapted.copy(binding = hiddenPolicy)))
    val hiddenObservation = SemanticStateInventory.canonical(JObject(parse(adapted.binding).asInstanceOf[JObject].obj
      .filterNot(_._1 == "native_state_observation")))
    intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(adapted.copy(binding = hiddenObservation)))
    println("SYNCHRONOUS_SOURCE_PROOF_PASS equations=13 missing_origin=1 entries=1 ordinary=1 hidden_policy=1 hidden_observation=1")
  }

  it should "reject observation layout corruption and reserved-port collisions without changing original selectors" in {
    val local = NativeCheckpointShellStateAccess.generate(source, selected.head).reference
    val ordinary = GeneratedStateBankAccess.generate(local, SimulatorStateInventory.discover(local), separateAccessClock = true)
    val adapted = NativeSynchronousMemoryReadout.generate(local, ordinary)
    assert(adapted.entries == ordinary.entries)
    val doc = parse(adapted.binding)
    val observation = doc \ "native_state_observation"
    val fields = (observation \ "state").asInstanceOf[JArray].arr
    val changes = Seq(
      "omitted" -> observation.transformField { case ("state", _) => "state" -> JArray(fields.tail) },
      "overlap" -> observation.transformField { case ("offset", _) => "offset" -> JInt(0) },
      "width" -> observation.transformField { case ("width", _) => "width" -> JInt(1) },
      "capture" -> observation.transformField { case ("complete_capture_owner", _) => "complete_capture_owner" -> JBool(true) },
      "restore" -> observation.transformField { case ("metasim_restore_supported", _) => "metasim_restore_supported" -> JBool(true) })
    changes.foreach { case (name, changed) =>
      val binding = SemanticStateInventory.canonical(doc.transformField {
        case ("native_state_observation", _) => "native_state_observation" -> changed
      })
      assert(intercept[IllegalArgumentException](GeneratedStateBankAccess.validateAnnotationBinding(
        adapted.copy(binding = binding))).getMessage.contains("changed source-bound native RAM"), name)
    }
    val root = local.circuit.modules.head.asInstanceOf[Module]
    val collision = ordinary.copy(state = ordinary.state.copy(circuit = ordinary.state.circuit.copy(modules = Seq(
      ordinary.state.circuit.modules.head.asInstanceOf[Module].copy(ports =
        ordinary.state.circuit.modules.head.ports :+ Port(NoInfo, NativeSynchronousStateObservation.port, Output, UIntType(IntWidth(1))))))))
    assert(intercept[IllegalArgumentException](NativeSynchronousStateObservation.generate(collision.state, ordinary.binding))
      .getMessage.contains("observation port identifier collision"))
    println("SYNCHRONOUS_OBSERVATION_PASS fields=14 bits=518 metadata_negatives=5 collision=1")
  }

  it should "translate correlated preservation and initialization owners only to their concrete copies" in {
    val h = source
    val q0 = selected.head.ref("ram")
    val q2 = top.instOf("q2", "SharedQueue").ref("ram")
    val init0 = firrtl.annotations.MemoryFileInlineAnnotation(q0, "q0.hex")
    val init2 = firrtl.annotations.MemoryFileInlineAnnotation(q2, "q2.hex")
    val keep = firrtl.transforms.DontTouchAnnotation(q0)
    val ptr0 = selected.head.ref("enq_ptr")
    val ptr2 = top.instOf("q2", "SharedQueue").ref("enq_ptr")
    val delay = midas.InternalXDCAnnotation(XDCFiles.Implementation,
      "set_max_delay 4 -from [get_cells {}_reg*] -to [get_cells {}_reg*]", ptr0, ptr2)
    val original = h.copy(annotations = h.annotations ++ Seq(init0, init2, keep, delay))
    val result = NativeCheckpointShellStateAccess.generate(original, selected.head)
    NativeCheckpointShellStateAccess.validate(result)
    val annotations = JsonProtocol.serialize(result.state.annotations)
    assert(annotations.contains("checkpoint_shell_selected_SharedQueue"))
    val inits = result.state.annotations.collect { case a: firrtl.annotations.MemoryFileInlineAnnotation => a }
    assert(inits.size == 2 && inits.contains(init2))
    assert(inits.find(_.target.serialize.contains("checkpoint_shell_selected_SharedQueue"))
      .exists(_.serialize.contains("q0.hex")))
    val constraints = result.state.annotations.collect { case a: midas.InternalXDCAnnotation => a }
    assert(constraints.size == 1 && constraints.head.argumentList.size == 2)
    assert(constraints.head.argumentList.last.serialize == ptr2.serialize)
    assert(constraints.head.argumentList.head.serialize.contains("checkpoint_shell_selected_SharedQueue"))
    val regions = NativeCheckpointRegions.generate(original, Seq.empty, shellTargets = Seq(selected.head))
    NativeCheckpointRegions.validate(regions)
    assert(regions.state.annotations.collect { case a: firrtl.annotations.MemoryFileInlineAnnotation => a }
      .exists(_ == init2))
  }

  it should "reject ambiguous module selection, stale paths, generated-name collisions and duplicate ownership" in {
    val h = source
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(h,
      ModuleTarget(h.circuit.main, "SharedQueue")))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(h,
      top.instOf("missing", "SharedQueue")))
    val original = h.circuit.modules.find(_.name == "SharedQueue").get.asInstanceOf[Module]
    val collision = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules :+
      original.copy(name = "checkpoint_shell_selected_SharedQueue")))
    assert(intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(collision, selected.head))
      .getMessage.contains("specialized definition collision"))
    intercept[IllegalArgumentException](NativeCheckpointRegions.generate(h, Seq.empty,
      shellTargets = Seq(selected.head, selected.head)))
    intercept[IllegalArgumentException](NativeCheckpointRegions.generate(h, Seq.empty))
  }

  it should "reject mismatched validif enables and mux, data-cast or secondary memory clocks" in {
    val h = source
    val clock = firrtl.WRef("clock", ClockType)
    val enq = firrtl.WRef("enq", UIntType(IntWidth(1)))
    val badClocks = Seq[Expression](
      ValidIf(firrtl.WRef("reset", UIntType(IntWidth(1))), clock, ClockType),
      Mux(enq, clock, clock, ClockType),
      DoPrim(firrtl.PrimOps.AsClock, Seq(DoPrim(firrtl.PrimOps.AsUInt, Seq(clock), Nil,
        UIntType(IntWidth(1)))), Nil, ClockType),
      firrtl.WRef("other_clock", ClockType))
    badClocks.foreach { bad =>
      def replace(s: Statement): Statement = s match {
        case n: DefNode if n.name == "writer_clock" => n.copy(value = bad)
        case x => x.mapStmt(replace)
      }
      val modules = h.circuit.modules.map {
        case m: Module if m.name == "SharedQueue" => m.copy(ports = m.ports :+
          Port(NoInfo, "other_clock", Input, ClockType), body = replace(m.body))
        case m: Module if m.name == h.circuit.main => m.copy(ports = m.ports :+
          Port(NoInfo, "other_clock", Input, ClockType), body = Block(Seq(m.body) ++
            (0 until 3).map(i => Connect(NoInfo, firrtl.WSubField(firrtl.WRef(s"q$i"),
              "other_clock", ClockType), firrtl.WRef("other_clock", ClockType)))))
        case m => m
      }
      val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(
        firrtl.passes.ToWorkingIR.run(h.circuit.copy(modules = modules))))
      val changed = h.copy(circuit = firrtl.passes.ResolveFlows.run(typed))
      intercept[IllegalArgumentException](NativeCheckpointRegions.generate(changed, Seq.empty,
        shellTargets = selected))
    }
  }
}
