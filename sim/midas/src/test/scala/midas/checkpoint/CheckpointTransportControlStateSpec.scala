// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{CircuitState, LowForm}
import firrtl.annotations.JsonProtocol
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointTransportControlStateSpec extends AnyFlatSpec {
  private lazy val input = Paths.get(sys.props("checkpoint.control.input"))
  private lazy val out = Paths.get(sys.props("checkpoint.control.out"))
  private def read(n: String) = new String(Files.readAllBytes(input.resolve(n)), UTF_8)
  private lazy val state = CircuitState(firrtl.passes.ToWorkingIR.run(
    firrtl.Parser.parse(read("reserved.fir"))), LowForm,
    JsonProtocol.deserialize(read("reserved-annotations.json")))
  behavior.of("Checkpoint transport control state observation")

  it should "enumerate all registers and preserve strict external-owner and emission rejection" in {
    val doc = CheckpointTransportControlState.inspect(state)
    val parsed = parse(doc)
    assert((parsed \ "bits") == JInt(483))
    assert((parsed \ "words") == JInt(8))
    assert((parsed \ "state").asInstanceOf[JArray].arr.size == 24)
    assert((parsed \ "deployable_capture_image") == JBool(false))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(state,
      "CheckpointPhysicalCaptureTransport"))
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(state))
    Files.createDirectories(out)
    Files.write(out.resolve("control-state.json"), doc.getBytes(UTF_8))
    Files.write(out.resolve("control-state-bank.sv"), CheckpointTransportControlState.readout(2).getBytes(UTF_8))
    Files.write(out.resolve("control-state-bank-3.sv"), CheckpointTransportControlState.readout(3).getBytes(UTF_8))
    Files.write(out.resolve("reserved.fir"), state.circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("reserved-annotations.json"), JsonProtocol.serialize(state.annotations).getBytes(UTF_8))
  }

  it should "cover wide specializations without word truncation or mask-clock reclassification" in {
    Seq(1, 2, 3, 31, 32, 65, 65535).foreach { gates =>
      val fields = CheckpointTransportControlState.fields(gates)
      assert(fields.map(_.width).sum == 481 + gates)
      assert(fields.sliding(2).forall { xs => xs.size != 2 || xs.head.offset + xs.head.width == xs.last.offset })
      assert(fields.filter(_.edge == "negedge clock").map(_.path) == Seq("physical.freeze_low"))
      assert(CheckpointTransportControlState.readout(gates).contains(s"word_index < ${(481 + gates + 63) / 64}"))
    }
    Seq(0, -1, 65536).foreach(g => intercept[IllegalArgumentException](CheckpointTransportControlState.fields(g)))
  }

  it should "reject substituted sources, duplicated definitions, reshaped ABI and invalid specializations" in {
    def reject(s: CircuitState) = intercept[IllegalArgumentException](CheckpointTransportControlState.inspect(s))
    def change(f: ExtModule => ExtModule) = state.copy(circuit = state.circuit.copy(modules =
      state.circuit.modules.map { case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" => f(m)
        case m => m }))
    val source = state.annotations.collectFirst { case a: firrtl.transforms.BlackBoxInlineAnno
      if a.name == "CheckpointCaptureCut.sv" => a }.get
    reject(state.copy(annotations = state.annotations.filterNot(_ == source)))
    reject(state.copy(annotations = state.annotations :+ source))
    reject(state.copy(annotations = state.annotations.map {
      case a if a == source => source.copy(text = source.text + "\n")
      case a => a
    }))
    reject(change(m => m.copy(ports = m.ports.map {
      case p if p.name == "control_state" => p.copy(tpe = UIntType(IntWidth(482)))
      case p => p
    })))
    reject(change(m => m.copy(params = m.params.map {
      case IntParam("GATES", _) => IntParam("GATES", 0)
      case p => p
    })))
    reject(change(m => m.copy(params = m.params :+ IntParam("UNKNOWN", 1))))
    reject(change(m => m.copy(params = m.params.map {
      case IntParam("OWNERS", _) => IntParam("OWNERS", 0)
      case p => p
    })))
    reject(state.copy(circuit = state.circuit.copy(modules = state.circuit.modules :+ state.circuit.modules.head)))
    reject(state.copy(form = firrtl.HighForm))
  }
}
