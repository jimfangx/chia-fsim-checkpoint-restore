// See LICENSE for license details.
package midas.core

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import chisel3._
import chisel3.stage.ChiselStage
import firrtl.{CircuitState, LowForm}
import firrtl.annotations.{JsonProtocol, ReferenceTarget}
import firrtl.ir.{Module => FirrtlModule, Input => FirrtlInput, Output => FirrtlOutput, _}
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.bridgeutils.{BridgeIOAnnotation, RationalClock}
import midas.checkpoint._
import midas.passes.fame.{FAMEChannelConnectionAnnotation, TargetClockChannel}
import midas.stage.GoldenGateOutputFileAnnotation
import midas.widgets._
import org.chipsalliance.cde.config.Parameters
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointWidgetReservationSpec extends AnyFlatSpec {
  private def parameters(binding: CheckpointCaptureTopBinding): Parameters = {
    val toCPU = new StreamAllocator
    val fromCPU = new StreamAllocator
    val ref = ReferenceTarget("Fixture", "Target", Nil, "clock_bits", Nil)
    val clock = FAMEChannelConnectionAnnotation("clock", TargetClockChannel(Seq(RationalClock("base", 1, 1)), Seq(1)),
      None, None, Some(Seq(ref)))
    val bridge = BridgeIOAnnotation(ReferenceTarget("Fixture", "Target", Nil, "clockbridge", Nil),
      Map.empty[String, String], None, classOf[CaptureTopFixtureClock].getName)
    (new midas.F2Config).toInstance.alterPartial {
      case SimWrapperKey => SimWrapperConfig(Seq(clock, bridge), Map.empty)
      case CheckpointCaptureTopKey => Some(binding)
      case ToCPUStreamAllocatorKey => toCPU
      case FromCPUStreamAllocatorKey => fromCPU
    }
  }
  private class Binding(missing: Boolean = false) extends NativeCheckpointWidgetReservationBinding(
      7, 2, Seq("fixture.functional", "fixture.host"), 2) {
    protected def connectOwners(top: FPGATopImp, capture: CheckpointCaptureSideband) = {
      capture.targetCycle := IO(Input(UInt(64.W))).suggestName("reservedCycle")
      capture.ownerPresent := IO(Input(UInt(2.W))).suggestName("reservedPresent")
      if (!missing) capture.ownerFrozen := IO(Input(UInt(2.W))).suggestName("reservedFrozen")
      capture.ownerError := IO(Input(UInt(2.W))).suggestName("reservedError")
      IO(Output(Bool())).suggestName("reservedFreeze") := capture.freeze
    }
  }
  private val out = Paths.get(sys.props("checkpoint.widget.out")).resolve("reservation")
  behavior.of("Native post-lowering checkpoint widget reservation")

  it should "reserve actual FPGATop resources and preserve explicit lowered endpoints without admitting an image" in {
    implicit val p: Parameters = parameters(new Binding)
    val top = LazyModule(new FPGATop)
    Files.createDirectories(out)
    val emitted = (new ChiselStage).execute(Array("-X", "low", "--target-dir", out.toString), Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => top.module)))
    val circuit = emitted.collectFirst { case firrtl.stage.FirrtlCircuitAnnotation(c) => c }.get
    val annotations = JsonProtocol.deserialize(new String(Files.readAllBytes(out.resolve("FPGATop.anno.json")), UTF_8))
    val state = CircuitState(circuit, LowForm, annotations)
    // Keep only actual live FIRRTL annotations in inspect/receipts; stage
    // metadata is not a contract for storage or capture ownership.
    val header = new StringBuilder; top.genHeader(header)
    val withHeader = state.copy(annotations = state.annotations :+
      GoldenGateOutputFileAnnotation(header.toString, ".const.h"))
    assert(top.checkpointCapture.get.toHostStreamIdx == 1 && top.toCPUStreamParams.map(_.idx) == Seq(0, 1))
    assert(top.streamingEngine.get.params.toCPUParams == top.toCPUStreamParams)
    assert(top.checkpointCapture.get.cfg.descriptors.isEmpty)
    intercept[IllegalArgumentException](CheckpointCaptureABI.header("FORGED", 0,
      top.checkpointCapture.get.cfg, 1))
    assert(header.toString.contains("#error Unbound_checkpoint_widget_reservation"))
    assert(!header.toString.contains("registry.add_widget(new checkpoint_capture_t"))
    val receipt = NativeCheckpointWidgetReservation.inspect(withHeader)
    assert((parse(receipt) \ "external_transport_state_owner") == JString("unsupported_no_state_adapter"))
    assert((parse(receipt) \ "complete_fpga_capture") == JBool(false))
    val pins = withHeader.annotations.collect { case a: NativeCheckpointWidgetPinAnnotation => a }
    assert(pins.size == 20)
    def reject(s: CircuitState): Unit = intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.inspect(s))
    reject(withHeader.copy(annotations = withHeader.annotations.filterNot(_ == pins.head)))
    reject(withHeader.copy(annotations = withHeader.annotations :+ pins.head))
    reject(withHeader.copy(annotations = withHeader.annotations.map {
      case a: NativeCheckpointWidgetPinAnnotation if a == pins.head => a.copy(role = "unknown")
      case a => a
    }))
    reject(withHeader.copy(annotations = withHeader.annotations.map {
      case a: NativeCheckpointWidgetPinAnnotation if a == pins.head => a.copy(target = a.target.copy(ref = "missing"))
      case a => a
    }))
    reject(withHeader.copy(annotations = withHeader.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(entries = a.entries + 1)
      case a => a
    }))
    reject(withHeader.copy(annotations = withHeader.annotations.map {
      case a: firrtl.transforms.BlackBoxInlineAnno if a.name == "CheckpointCaptureCut.sv" => a.copy(text = a.text + "\n")
      case a => a
    }))
    reject(withHeader.copy(circuit = withHeader.circuit.copy(modules = withHeader.circuit.modules.map {
      case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" =>
        m.copy(ports = m.ports.filterNot(_.name == "dma_abort"))
      case m => m
    })))
    def change(f: firrtl.ir.Statement => firrtl.ir.Statement): CircuitState = withHeader.copy(
      circuit = withHeader.circuit.copy(modules = withHeader.circuit.modules.map {
        case m: FirrtlModule =>
          def walk(s: firrtl.ir.Statement): firrtl.ir.Statement = f(s.mapStmt(walk))
          m.copy(body = walk(m.body))
        case m => m
      }))
    val outputPin = pins.find(_.role == "response_data").get.target.ref
    reject(change {
      case c: Connect if c.loc.serialize == outputPin => c.copy(expr = UIntLiteral(1, IntWidth(64)))
      case n: DefNode if n.name == outputPin => n.copy(value = UIntLiteral(1, IntWidth(64)))
      case s => s
    })
    val inputPin = pins.find(_.role == "reset").get.target.ref
    reject(change {
      case c: Connect if c.loc.serialize == inputPin => c.copy(expr = UIntLiteral(0, IntWidth(1)))
      case n: DefNode if n.name == inputPin => n.copy(value = UIntLiteral(0, IntWidth(1)))
      case s => s
    })
    val withoutMarkers = withHeader.copy(annotations = Nil)
    assert(intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(withoutMarkers))
      .getMessage.contains("external transport state owner"))
    val withoutSentinel = withHeader.copy(circuit = withHeader.circuit.copy(modules =
      withHeader.circuit.modules.map {
        case m: ExtModule => m.copy(params = m.params.map {
          case IntParam("MANIFEST_SHA256", _) => IntParam("MANIFEST_SHA256", 1)
          case p => p
        })
        case m => m
      }))
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(withoutSentinel))
    val headerOnly = withoutMarkers.copy(circuit = withoutSentinel.circuit,
      annotations = Seq(GoldenGateOutputFileAnnotation(header.toString, ".const.h")))
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(headerOnly))
    intercept[IllegalArgumentException](SimulatorStateInventory.discover(withHeader.copy(annotations = Nil)))
    val opaque = intercept[IllegalArgumentException](SimulatorStateInventory.discover(
      withHeader, "CheckpointPhysicalCaptureTransport"))
    assert(opaque.getMessage.contains("opaque external module CheckpointPhysicalCaptureTransport") &&
      opaque.getMessage.contains("no exact state adapter"))
    // Both local module targets after linking and absolute target paths are
    // supported. Shared host definitions still reject instead of aliasing pins.
    val originalRoot = circuit.modules.find(_.name == circuit.main).get
    val parent = FirrtlModule(NoInfo, "ReservationShell", originalRoot.ports,
      Block(Seq(firrtl.WDefInstance(NoInfo, "top", circuit.main, firrtl.Utils.module_type(originalRoot))) ++
        originalRoot.ports.map { p =>
          val pin = firrtl.WSubField(firrtl.WRef("top"), p.name, p.tpe)
          val port = firrtl.WRef(p.name, p.tpe)
          if (p.direction == FirrtlInput) Connect(NoInfo, pin, port) else Connect(NoInfo, port, pin)
        }))
    val renames = firrtl.RenameMap(Map(firrtl.annotations.CircuitName(circuit.main) ->
      Seq(firrtl.annotations.CircuitName("ReservationShell"))))
    val nested = withHeader.copy(circuit = circuit.copy(main = "ReservationShell", modules = circuit.modules :+ parent),
      annotations = withHeader.annotations.flatMap(_.update(renames)))
    assert((parse(NativeCheckpointWidgetReservation.inspect(nested)) \ "host") == JString("ReservationShell.top"))
    val absolute = nested.copy(annotations = nested.annotations.map {
      case a: NativeCheckpointWidgetPinAnnotation => a.copy(target =
        firrtl.annotations.ModuleTarget("ReservationShell", "ReservationShell").instOf("top", "FPGATop").ref(a.target.ref))
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(target =
        firrtl.annotations.ModuleTarget("ReservationShell", "ReservationShell").instOf("top", "FPGATop").instOf("sim", "SimWrapper"))
      case a => a
    })
    NativeCheckpointWidgetReservation.inspect(absolute)
    reject(nested.copy(circuit = nested.circuit.copy(modules = nested.circuit.modules.map {
      case m: FirrtlModule if m.name == parent.name => m.copy(body = Block(Seq(m.body,
        firrtl.WDefInstance("second", "FPGATop"))))
      case m => m
    })))
    val inlined = midas.passes.ResolveSimulationInline(withHeader.copy(annotations =
      withHeader.annotations.filterNot(_.isInstanceOf[firrtl.stage.RunFirrtlTransformAnnotation]) :+
        firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.passes.InlineInstances)))
    assert(inlined.annotations.count(_.isInstanceOf[NativeCheckpointWidgetPinAnnotation]) == 20)
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(inlined))
    // The current inspector is deliberately bounded to the concrete-widget
    // boundary. Native-inline promotion needs its own complete peer contract.
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.inspect(inlined))
    Files.write(out.resolve("native-inlined.fir"), inlined.circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("native-inlined-annotations.json"), JsonProtocol.serialize(inlined.annotations).getBytes(UTF_8))
    Files.write(out.resolve("reserved.fir"), circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("reserved-annotations.json"), JsonProtocol.serialize(withHeader.annotations).getBytes(UTF_8))
    Files.write(out.resolve("reservation.json"), receipt.getBytes(UTF_8))
    Files.write(out.resolve("reserved.h"), header.toString.getBytes(UTF_8))
    val verilog = new firrtl.stage.transforms.Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]),
      firrtl.stage.Forms.LowForm).execute(withHeader.copy(annotations = withHeader.annotations :+
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))).getEmittedCircuit.value
    Files.write(out.resolve("reserved.sv"), verilog.getBytes(UTF_8))
    println(s"NATIVE_WIDGET_RESERVATION out=$out pins=${pins.size} checkpoint_stream=1 unsupported_transport_owner=true")
  }

  it should "reject a missing owner and forged reservation parameters" in {
    implicit val p: Parameters = parameters(new Binding(missing = true))
    val top = LazyModule(new FPGATop)
    val error = intercept[Exception] {
      (new ChiselStage).emitVerilog(top.module, Array("--target-dir", out.resolve("missing-owner").toString))
    }
    assert(error.getMessage.contains("not fully initialized") && error.getMessage.contains("ownerFrozen"))
    intercept[IllegalArgumentException](CheckpointCaptureWidgetParams(7, "1" * 64,
      Seq("fixture"), physicalClockGates = 2, deferredGraft = true))
  }
}
