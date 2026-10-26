// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths, StandardCopyOption}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.passes.ResolveSimulationInline
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** Stateful native subtree inside the ACTUAL reserved FPGATop/MMIO/DMA host.
  * The reservation's public ports and all non-selected execution are retained.
  */
class NativeCheckpointWidgetGraftSpec extends AnyFlatSpec {
  import SemanticStateInventory.{canonical, sha256}
  private lazy val input = Paths.get(sys.props("checkpoint.peer.input"))
  private lazy val out = Paths.get(sys.props("checkpoint.graft.out"))
  private def read(name: String): String = new String(Files.readAllBytes(input.resolve(name)), UTF_8)
  private def resolved(c: Circuit): Circuit = {
    val w = firrtl.passes.ToWorkingIR.run(c)
    firrtl.passes.ResolveFlows.run(firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(w)))
  }
  private lazy val original = CircuitState(resolved(Parser.parse(read("reserved.fir"))), LowForm,
    JsonProtocol.deserialize(read("reserved-annotations.json")))
  private lazy val source = {
    val native = NativeCheckpointInPlaceFixture.reference
    val old = original.circuit.modules.find(_.name == "SimWrapper").get.asInstanceOf[Module]
    val text = "circuit SimWrapper :\n  module SimWrapper :\n" +
      old.ports.map(p => "    " + p.serialize).mkString("\n") + "\n" +
      """    channelPorts_clock_ready <= UInt<1>(1)
    inst network of NativeStorage
    network.clock <= clock
    network.reset <= reset
    network.enable <= UInt<2>(3)
    network.address <= bits(network.cycle, 2, 0)
    network.data <= pad(network.cycle, 130)
    network.write <= UInt<1>(1)
"""
    val wrapper = Parser.parse(text).modules.head.asInstanceOf[Module].copy(ports = old.ports)
    val circuit = native.circuit.copy(main = "SimWrapper", modules = native.circuit.modules :+ wrapper)
    val low = new Compiler(Forms.LowForm).execute(CircuitState(circuit, ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
    assert(low.annotations.toVector == Vector(firrtl.transforms.NoCircuitDedupAnnotation), low.annotations.mkString("\n"))
    NativeCheckpointInPlaceContext.generate(low,
      ModuleTarget("SimWrapper", "SimWrapper").instOf("network", "NativeStorage"))
  }
  private lazy val before = {
    val native = source.host.circuit.modules
    val names = native.map(_.name).toSet
    assert(original.circuit.modules.filter(m => names(m.name)).map(_.name) == Seq("SimWrapper"))
    // This is the explicit bounded replacement fixture BEFORE the native pass.
    // Its old TargetBox marker names the removed fixture target and cannot be
    // silently carried into the new source epoch.
    val annotations = original.annotations.filterNot(a =>
      a.isInstanceOf[midas.core.TargetBoxAnnotation] || a.isInstanceOf[firrtl.stage.RunFirrtlTransformAnnotation]) :+
      firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.passes.InlineInstances)
    original.copy(circuit = resolved(original.circuit.copy(modules =
      original.circuit.modules.filterNot(m => m.name == "SimWrapper") ++ native)), annotations = annotations)
  }
  private lazy val after = ResolveSimulationInline(before)
  private lazy val connected = NativeCheckpointWidgetGraft.connect(before, after, source)
  private val failures = scala.collection.mutable.ArrayBuffer.empty[String]
  private def reject(beforeInput: CircuitState = before, afterInput: CircuitState = after,
      sourceInput: NativeCheckpointInPlaceContext.Result = source, fragment: String): Unit = {
    val error = intercept[IllegalArgumentException](NativeCheckpointWidgetGraft.connect(beforeInput, afterInput, sourceInput))
    assert(error.getMessage.contains(fragment), error.getMessage)
    failures += error.getMessage
  }
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))
  behavior.of("Checked existing-execution widget graft connection")

  it should "connect all twenty routes inside the existing host with original metadata and admission guards" in {
    val result = connected
    val doc = parse(result.binding)
    assert((doc \ "pins").asInstanceOf[JArray].arr.size == 20)
    assert((doc \ "descriptor_identity_bound") == JBool(false))
    assert(result.state.annotations == after.annotations)
    assert(result.state.circuit.modules.find(_.name == after.circuit.main).get.ports ==
      after.circuit.modules.find(_.name == after.circuit.main).get.ports)
    val live = GeneratedExternalOwners.instances(result.state, result.state.circuit.main)
    assert(live.count(_._2.name == "SimWrapper") == 1)
    assert(live.count { case (_, m: ExtModule) => m.defname == "BUFGCE"; case _ => false } == 3)
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(result.state))
    assert(intercept[IllegalArgumentException](PhysicalClockGateContract.requireCaptureOwner(result.state))
      .getMessage.contains("synthesized capture owner and common low-phase cut"))
    val descriptors = NativeCheckpointInPlaceAccess.descriptors(source)
    assert(descriptors.entries.size == 7 && descriptors.physicalClockGates == 2)
    Files.createDirectories(out)
    Seq("before-reserved" -> before, "native-inlined" -> after,
      "original-selected" -> source.host, "graft" -> result.graft.state, "connected" -> result.state).foreach {
      case (name, s) => save(name + ".fir", s.circuit.serialize)
        save(name + "-annotations.json", JsonProtocol.serialize(s.annotations))
    }
    save("connection.json", result.binding)
    save("peer-routing.json", NativeCheckpointWidgetPeerRouting.inspect(before, after))
    save("descriptors.json", descriptors.manifest)
    save("descriptor-binding.json", descriptors.binding)
    save("source-context.json", source.binding)
    result.graft.files.foreach(f => save("graft" + f.fileSuffix, f.body))
    Seq("reserved.h", "CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv",
      "CheckpointCaptureTransport.sv", "CheckpointCaptureCut.sv").foreach(n =>
      Files.copy(input.resolve(n), out.resolve(n), StandardCopyOption.REPLACE_EXISTING))
    val emitter = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
    val verilog = emitter.execute(result.state.copy(annotations = result.state.annotations.filterNot(
      _.isInstanceOf[firrtl.EmitCircuitAnnotation]) :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      .getEmittedCircuit.value
    save("connected.sv", verilog)
    println(s"NATIVE_WIDGET_GRAFT out=$out entries=7 gates=2 routes=20 guards_retained=true")
  }

  it should "reject stale peer history and source/circuit/annotation substitutions before connecting" in {
    reject(afterInput = after.copy(circuit = after.circuit.copy(info = FileInfo("changed"))), fragment = "stale inline")
    reject(sourceInput = source.copy(host = source.host.copy(circuit = source.host.circuit.copy(main = "NativeStorage"))),
      fragment = "source root")
    reject(sourceInput = source.copy(host = source.host.copy(annotations = Nil)), fragment = "annotation projection")
    reject(sourceInput = source.copy(host = source.host.copy(circuit = source.host.circuit.copy(modules =
      source.host.circuit.modules.map {
        case m: Module if m.name == "SimWrapper" => m.copy(info = FileInfo("changed"))
        case m => m
      }))), fragment = "original selected definition differs")
    reject(sourceInput = source.copy(state = source.state.copy(circuit =
      source.state.circuit.copy(info = FileInfo("changed")))), fragment = "instrumented circuit changed")
    reject(sourceInput = source.copy(native = source.native.copy(binding = source.native.binding + "\n")),
      fragment = "source projection binding changed")
    // Preserve the complete reservation guard after connecting, even if a
    // caller discards every sidecar or marker in an attempted output emission.
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(
      connected.state.copy(annotations = Nil)))
    save("negative-tests.json", canonical(JObject(List("rejections" -> JArray(failures.map(JString(_)).toList),
      "physical_fpga_capture" -> JBool(false)))))
  }

  it should "reject actual descriptor count, sharing, collision and selected annotation conflicts" in {
    def fault(s: CircuitState, fragment: String): Unit =
      reject(beforeInput = s, afterInput = ResolveSimulationInline(s), fragment = fragment)
    val entries = before.copy(circuit = before.circuit.copy(modules = before.circuit.modules.map {
      case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" => m.copy(params = m.params.map {
        case IntParam("ENTRIES", n) => IntParam("ENTRIES", n + 1)
        case p => p
      })
      case m => m
    }), annotations = before.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(entries = a.entries + 1)
      case a => a
    })
    fault(entries, "reserved entry/gate count differs")
    val shared = before.copy(circuit = before.circuit.copy(modules = before.circuit.modules.map {
      case m: Module if m.name == before.circuit.main =>
        val selected = source.host.circuit.modules.find(_.name == "SimWrapper").get
        val instance = firrtl.WDefInstance(NoInfo, "another_sim", "SimWrapper", firrtl.Utils.module_type(selected))
        m.copy(body = Block(Seq(m.body, instance) ++ selected.ports.filter(_.direction == Input).map { p =>
          val value: Expression = if (p.tpe == ClockType) firrtl.WRef("clock", ClockType)
            else UIntLiteral(0, p.tpe.asInstanceOf[UIntType].width)
          Connect(NoInfo, firrtl.WSubField(firrtl.WRef("another_sim"), p.name, p.tpe), value)
        }))
      case m => m
    }))
    fault(shared, "selected definition must have one concrete execution")
    fault(before.copy(circuit = before.circuit.copy(modules = before.circuit.modules :+
      Module(NoInfo, "SimWrapperCheckpointReadoutAdapter", Nil, EmptyStmt))), "generated module collides")
    fault(before.copy(annotations = before.annotations :+ firrtl.transforms.DontTouchAnnotation(
      ModuleTarget(before.circuit.main, "SimWrapper").ref("channelPorts_clock_ready"))),
      "host annotation targets changed selected definition")
    save("negative-tests.json", canonical(JObject(List("rejections" -> JArray(failures.map(JString(_)).toList),
      "physical_fpga_capture" -> JBool(false)))))
  }
}
