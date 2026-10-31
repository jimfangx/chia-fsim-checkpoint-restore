// See LICENSE for license details.
package midas.widgets

import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.ir.{ExtModule, IntParam}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import freechips.rocketchip.diplomacy.LazyModule
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.checkpoint._
import midas.stage.GoldenGateOutputFileAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

object CheckpointCaptureNativeWidgetFixture {
  private val source = """circuit NativeBindingFixture :
    |  module NativeBindingFixture :
    |    input clock : Clock
    |    input reset : UInt<1>
    |    output value : UInt<64>
    |    reg state : UInt<64>, clock with :
    |      reset => (reset, UInt<64>(0))
    |    state <= add(state, UInt<1>(1))
    |    value <= state
    |""".stripMargin
  private val state = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm))
    .copy(form = LowForm)
  private val inventory = SemanticStateInventory.discover(state, state.circuit.main)
  private val generated = GeneratedStateBankAccess.generate(state, inventory)
  val original = CheckpointCaptureDescriptors.fromGenerated(GeneratedStateBankAccess.generate(state, inventory,
    bindings = Some(generated.entries.map(_.copy(bank = 1)))), inventory)
  val descriptors = CheckpointNativeCaptureDescriptors.bind(original, 130, 3,
    Seq("fixture.functional", "fixture.host"))
  val cfg = CheckpointCaptureWidgetParams(descriptors.entries.size, descriptors.manifestSha256,
    descriptors.ownerNames, queueDepth = 2, physicalClockGates = descriptors.endpoint.gates,
    nativeDescriptors = Some(descriptors))
}

class CheckpointCaptureNativeWidgetSpec extends AnyFlatSpec {
  import CheckpointCaptureNativeWidgetFixture._
  private val out = Paths.get(sys.props("checkpoint.widget.out")).resolve("native-widget")
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))
  behavior.of("Installed native checkpoint widget")

  it should "require the full bound identity and reject the deferred physical reservation ABI" in {
    intercept[IllegalArgumentException](cfg.copy(entries = cfg.entries + 1))
    intercept[IllegalArgumentException](cfg.copy(manifestSha256 = original.manifestSha256))
    intercept[IllegalArgumentException](cfg.copy(ownerNames = cfg.ownerNames.reverse))
    intercept[IllegalArgumentException](cfg.copy(physicalClockGates = 2))
    intercept[IllegalArgumentException](cfg.copy(descriptors = Some(original)))
    intercept[IllegalArgumentException](cfg.copy(deferredGraft = true))
    assert(descriptors.cppNativeBinding ==
      "checkpoint_native_capture_binding_t{1U, 130U, 3U, 2U, 1U, 252U, 251U, 2U, 1585U}")
    val noControl = CheckpointNativeCaptureDescriptors.bind(original, 63, 3, cfg.ownerNames,
      captureNativeControl = false, snapshotBank = 248, nativeControlBank = 249)
    val small = cfg.copy(entries = noControl.entries.size, manifestSha256 = noControl.manifestSha256,
      nativeDescriptors = Some(noControl))
    assert(small.entries == 2 && small.physicalClockGates == 3)
    assert(noControl.cppNativeBinding ==
      "checkpoint_native_capture_binding_t{1U, 63U, 3U, 2U, 0U, 248U, 249U, 2U, 1518U}")
    assert(CheckpointCaptureABI.header("SMALL", 0, small, 0).contains("SMALL_ENTRIES 2U"))
  }

  it should "select the actual native endpoint and install its complete stream descriptor driver identity" in {
    implicit val p: Parameters = (new midas.F2Config).toInstance
    val np = p(CtrlNastiKey)
    assert(np.dataBits == 32 && np.addrBits == 32 && np.idBits == 12)
    val widget = LazyModule(new CheckpointCaptureWidget(cfg))
    val rtl = new ChiselStage().emitVerilog(widget.module, Array("--target-dir", out.toString))
    val header = new StringBuilder
    widget.module.genHeader(BigInt(4096), Map.empty, header)
    val h = header.toString
    assert(h.contains(NativeCheckpointWidgetIdentity.incomplete))
    assert(h.contains(descriptors.cppManifest) && h.contains(descriptors.cppNativeBinding))
    assert(h.contains("new checkpoint_capture_t(simif, registry.get_stream_engine(),"))
    assert(!h.contains("new checkpoint_capture_t(simif, *registry.get_stream_engine(),"))
    assert(h.contains(s"TO_HOST_STREAM_IDX ${widget.toHostStreamIdx}U") && h.contains("ENTRIES 4U"))
    assert(h.contains("NATIVE_ORIGINAL_ROM_ENTRIES 1U") && h.contains("NATIVE_STREAM_ENTRIES 4U"))
    assert(h.contains("SHADOW_CONTROL_BITS 827U") && h.contains("SHADOW_BITS 1585U"))
    assert(h.contains(descriptors.manifestSha256) && h.contains(descriptors.bindingSha256))
    assert(!h.contains("Unbound_checkpoint_widget_reservation"))
    CheckpointNativeCaptureTransportBlackBox.resources.foreach(n => assert(h.contains(n + " SHA256:")))
    assert(rtl.contains("capture_native_readerIdle") && rtl.contains("capture_native_liveState"))
    assert(rtl.contains("capture_native_functionalHold") && rtl.contains("capture_native_snapshotState"))
    assert(rtl.contains("capture_native_rawControlState") && rtl.contains("capture_native_nativeControlBankState"))
    assert(rtl.contains("nativeEndpoint") && rtl.contains("originalROM"))
    assert(!rtl.contains("CheckpointPhysicalCaptureTransport #("))
    assert(!rtl.contains(".RESTORE_ENABLE("))
    val circuit = Parser.parse(Files.readString(out.resolve("CheckpointCaptureWidget.fir")))
    for (name <- Seq("CheckpointCaptureWidget", "CheckpointNativeBoundWidgetEndpoint",
        "CheckpointNativeWidgetEndpoint", "CheckpointNastiAdapter")) {
      val module = circuit.modules.find(_.name == name).get
      for (channel <- Seq("aw", "ar", "b", "r")) {
        val ioType = module.ports.find(_.name == "io").get.tpe
        val idType = Seq("ctrl", channel, "bits", "id").foldLeft(ioType) { (tpe, field) =>
          tpe.asInstanceOf[firrtl.ir.BundleType].fields.find(_.name == field).get.tpe
        }
        assert(idType == firrtl.ir.UIntType(firrtl.ir.IntWidth(12)), s"$name $channel ${idType.serialize}")
      }
    }
    val native = circuit.modules.collectFirst { case e: ExtModule
      if e.defname == "CheckpointNativeCaptureTransport" => e }.get
    val params = native.params.collect { case IntParam(n, v) => n -> v }.toMap
    assert(params("ENTRIES") == 1 && params("MANIFEST_SHA256") == BigInt(descriptors.manifestSha256, 16))
    assert(params("BITS") == 130 && params("GATES") == 3 && params("OWNERS") == 2)
    assert(params("EXTERNAL_CLOCK_NETWORK") == 1 && params("CAPTURE_NATIVE_CONTROL") == 1)
    val state = CircuitState(circuit, LowForm, Seq(GoldenGateOutputFileAnnotation(h, ".const.h")))
    Seq(state, state.copy(annotations = Nil)).foreach { s =>
      assert(intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(s))
        .getMessage.contains("native transport requires complete atomic capture/restore owners"))
      assert(intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.inspect(s))
        .getMessage.contains("physical deferred reservation/peer ABI"))
    }
    assert(intercept[IllegalArgumentException](SimulatorStateInventory.discover(state.copy(annotations = Nil),
      "CheckpointNativeCaptureTransport")).getMessage.contains("no exact state adapter"))
    save("manifest.json", descriptors.manifest); save("binding.json", descriptors.binding)
    save("manifest.inc", descriptors.cppManifest); save("constants.h", h)
    save("native-constants.h", descriptors.header("NATIVE"))
    save("endpoint-observation.json", descriptors.endpoint.observationBinding)
    println(s"NATIVE_CAPTURE_WIDGET_ELABORATED out=$out original=1 stream=4 bits=130 gates=3 owners=2")
  }
}
