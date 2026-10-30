// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, Parser}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.targetutils.xdc.{RAMStyleHint, RAMStyles, XDCAnnotation, XDCFiles}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** Exact emitted Queue_56 geometry, isolated from the enclosing simulator.
  * The synthetic four-lane host is explicitly a component, never CPU evidence.
  */
class RocketNativeQueueCaptureSpec extends AnyFlatSpec {
  behavior.of("Exact Rocket native queue capture geometry")
  it should "retain four original ULTRA queues and every native observation owner" in {
    val path = Paths.get(sys.props("checkpoint.shell.rocket.source") + ".fir")
    val text = new String(Files.readAllBytes(path), UTF_8)
    val original = Parser.parse(text).modules.collectFirst { case m: Module if m.name == "Queue_56" => m }.get
    assert(original.body.serialize.contains("depth => 6144"))
    assert(original.body.serialize.contains("data-type => UInt<512>"))
    val fields = original.ports.filterNot(p => p.name == "clock" || p.name == "reset")
    val ports = (0 until 4).flatMap(q => fields.map(p =>
      s"    ${if (p.direction == Input) "input" else "output"} lane${q}_${p.name} : ${p.tpe.serialize}")).mkString("\n")
    val lanes = (0 until 4).map { q =>
        s"    inst q$q of Queue_56\n    q$q.clock <= clock\n    q$q.reset <= reset\n" + fields.map { p =>
          if (p.direction == Input) s"    q$q.${p.name} <= lane${q}_${p.name}"
          else s"    lane${q}_${p.name} <= q$q.${p.name}"
        }.mkString("\n")
    }.mkString("\n")
    val host = Parser.parse("circuit RocketQueueHost :\n  module RocketQueueHost :\n    input clock : Clock\n    input reset : UInt<1>\n" + ports + "\n    inst gate of Native\n    gate.I <= clock\n    gate.CE <= UInt<1>(1)\n" + lanes + "\n")
      .modules.head.asInstanceOf[Module]
    val primitive = Parser.parse("circuit N :\n  extmodule N :\n    input I : Clock\n    input CE : UInt<1>\n    output O : Clock\n    defname = BUFGCE\n").modules.head.asInstanceOf[ExtModule].copy(name = "Native")
    val circuit = Circuit(NoInfo, Seq(primitive, original, host), "RocketQueueHost")
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val hint = XDCAnnotation(XDCFiles.Synthesis, RAMStyleHint.propertyTemplate(RAMStyles.ULTRA),
      ModuleTarget("RocketQueueHost", "Queue_56").ref("ram"))
    val state = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation, hint))
    val targets = (0 until 4).map(q => ModuleTarget("RocketQueueHost", "RocketQueueHost").instOf(s"q$q", "Queue_56"))
    val result = NativeCheckpointRegions.generate(state, Seq.empty, shellTargets = targets)
    NativeCheckpointRegions.validate(result)
    assert(result.shells.size == 4 && result.descriptors.entries.size == 20)
    assert(result.shells.forall(s => (parse(s.banks.binding) \ "native_state_observation" \ "bits") == JInt(1664)))
    assert(result.shells.forall(s => (parse(s.banks.binding) \ "native_state_observation" \ "state").asInstanceOf[JArray].arr.size == 14))
    assert((parse(result.descriptors.manifest) \ "uncovered_functional_state") == JArray(Nil))
    assert((parse(result.descriptors.manifest) \ "complete_fpga_capture") == JBool(false))
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("rocket-native-queues")
    Files.createDirectories(out)
    def save(n: String, s: String): Unit = { Files.write(out.resolve(n), s.getBytes(UTF_8)); () }
    def emit(s: CircuitState): String = {
      val constrained = midas.passes.WriteXDCFile.execute(s.copy(annotations = s.annotations :+
        midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)))
      val emitter = new SemanticCleanEmitter
      new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
        .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
        .getEmittedCircuit.value
    }
    save("source.fir", state.circuit.serialize); save("source.sv", emit(state))
    save("production-source-sha256.txt", SemanticStateInventory.sha256(text) + "\n")
    save("production-queue.fir", original.serialize)
    save("generated.fir", result.state.circuit.serialize); save("generated.sv", emit(result.state))
    save("generated.anno.json", JsonProtocol.serialize(result.state.annotations))
    save("binding.json", result.binding); save("descriptors.json", result.descriptors.manifest)
    save("routing.json", result.routing.binding)
    val constrained = midas.passes.WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)))
    constrained.annotations.collect { case a: midas.passes.XDCOutputAnnotation => a }.foreach(a => save("generated" + a.suffix.get, a.fileBody))
    result.shells.zipWithIndex.foreach { case (s, q) =>
      save(s"shell-$q-banked.fir", s.banks.state.circuit.serialize)
      save(s"shell-$q-banks-binding.json", s.banks.binding)
      save(s"shell-$q-mapping.json", s.mapping)
    }
    println(s"ROCKET_NATIVE_QUEUE_GEOMETRY out=$out lanes=4 bits=6656 descriptors=20")
  }
}
