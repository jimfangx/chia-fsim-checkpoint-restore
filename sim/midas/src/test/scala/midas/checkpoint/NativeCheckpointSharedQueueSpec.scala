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
    }
    val damaged = result.state.copy(circuit = result.state.circuit.copy(info = FileInfo("changed")))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(result.copy(state = damaged)))
    println(s"SHARED_QUEUE_ACCESS out=$out selected=2 untouched=1 descriptors=10")
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
