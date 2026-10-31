// See LICENSE for license details.
package midas.widgets

import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.ir.{ExtModule, IntParam, Module => FirrtlModule}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.checkpoint.{GeneratedStateBankAccess, SemanticStateInventory, SimulatorStateInventory,
  PhysicalClockCutPlan, PhysicalClockCutDependencies}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointCaptureNativeBindingSpec extends AnyFlatSpec {
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
  private def original(bank: Int = 1): CheckpointCaptureDescriptors =
    CheckpointCaptureDescriptors.fromGenerated(GeneratedStateBankAccess.generate(state, inventory,
      bindings = Some(generated.entries.map(_.copy(bank = bank)))), inventory)
  private val owners = Seq("fixture.functional", "fixture.host")
  private def bound(bits: Int = 130, capture: Boolean = true, snapshot: Int = 252, control: Int = 251) =
    CheckpointNativeCaptureDescriptors.bind(original(), bits, 3, owners, capture, snapshot, control)
  private val out = Paths.get(sys.props("checkpoint.widget.out")).resolve("native-binding")
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))

  behavior.of("Checked original-ROM and appended native stream binding")

  it should "retain original identities and exact control/full/tail order across word boundaries" in {
    for (bits <- Seq(1, 63, 64, 65, 128, 130); capture <- Seq(false, true)) {
      val b = bound(bits, capture)
      assert(b.entries.head == b.original.entries.head && b.endpoint.entries == 1)
      val appended = b.entries.drop(1).map(e => (e.bank, e.slot, e.width, e.depth))
      val control = if (capture) Seq((251, 0, 64, (bits + 3 + 150 + 63) / 64)) else Nil
      val full = if (bits >= 64) Seq((252, 0, 64, bits / 64)) else Nil
      val tail = if (bits % 64 != 0) Seq((252, 1, bits % 64, 1)) else Nil
      assert(appended == control ++ full ++ tail)
      assert(b.endpoint.totalEntries == b.entries.size && b.endpoint.manifestSha256 == b.manifestSha256)
      assert(b.entries.map(_.stateID).distinct.size == b.entries.size)
      assert(bound(bits, capture).binding == b.binding)
    }
    assert(bound(snapshot = 248, control = 249).entries.drop(1).map(_.bank) == Seq(249, 248, 248))
  }

  it should "bind original provenance, all source bytes, owner order and every ABI option" in {
    val b = bound(); val m = parse(b.manifest); val d = parse(b.binding)
    assert((d \ "original_manifest") == parse(b.original.manifest))
    assert((d \ "original_binding") == parse(b.original.binding))
    assert((d \ "manifest") == m && (d \ "manifest_sha256") == JString(b.manifestSha256))
    assert((m \ "original_binding_sha256") == JString(b.original.bindingSha256))
    CheckpointNativeCaptureTransportBlackBox.resources.foreach { name =>
      val in = getClass.getResourceAsStream("/" + name)
      val text = try new String(in.readAllBytes(), UTF_8) finally in.close()
      assert((m \ "sources" \ name) == JString(SemanticStateInventory.sha256(text)))
    }
    Seq(bound(65), bound(capture = false), bound(snapshot = 248), bound(control = 249),
      CheckpointNativeCaptureDescriptors.bind(original(2), 130, 3, owners),
      CheckpointNativeCaptureDescriptors.bind(original(), 130, 3, owners.reverse)).foreach { other =>
      assert(other.manifestSha256 != b.manifestSha256 && other.bindingSha256 != b.bindingSha256)
      intercept[IllegalArgumentException](b.checkEndpoint(other.endpoint))
    }
    b.checkEndpoint(b.endpoint)
    intercept[IllegalArgumentException](b.checkEndpoint(b.endpoint.copy(manifestSha256 = "2" * 64)))
  }

  it should "reject reserved aliases even with control disabled and reject invalid geometry or owners" in {
    for (bank <- Seq(251, 252); capture <- Seq(false, true)) {
      intercept[IllegalArgumentException] {
        CheckpointNativeCaptureDescriptors.bind(original(bank), 130, 3, owners, capture)
      }
    }
    Seq(0, -1, Int.MaxValue).foreach(n => intercept[IllegalArgumentException](bound(n)))
    Seq(-1, 255, 256).foreach(n => {
      intercept[IllegalArgumentException](bound(snapshot = n))
      intercept[IllegalArgumentException](bound(control = n))
    })
    intercept[IllegalArgumentException](bound(snapshot = 251))
    Seq(Seq.empty[String], Seq("same", "same"), Seq("newline\nowner"), Seq("bad*/owner")).foreach { names =>
      intercept[IllegalArgumentException](CheckpointNativeCaptureDescriptors.bind(original(), 130, 3, names))
    }
    Seq(0, -1, 65536).foreach(n => intercept[IllegalArgumentException] {
      CheckpointNativeCaptureDescriptors.bind(original(), 130, n, owners)
    })
  }

  it should "emit little-endian full host metadata while keeping every ownership deficit explicit" in {
    val b = bound(); val m = parse(b.manifest); val d = parse(b.binding)
    val bytes = b.manifestSha256.grouped(2).toSeq.reverse.map("0x" + _).mkString(", ")
    assert(b.cppManifest.startsWith("checkpoint_capture_manifest_t{{{" + bytes + "}}, "))
    assert(b.cppManifest.contains("{251U, 0U, 64U, 5U}, {252U, 0U, 64U, 2U}, {252U, 1U, 2U, 1U}"))
    val h = b.header("NATIVE")
    assert(h.contains("NATIVE_ORIGINAL_ROM_ENTRIES 1U") && h.contains("NATIVE_STREAM_ENTRIES 4U"))
    assert(h.contains("NATIVE_EXTERNAL_OWNERS 2U") && h.contains("NATIVE_COORDINATOR_OWNERS 3U"))
    assert(h.contains("NATIVE_SHADOW_CONTROL_BITS 827U") && h.contains("NATIVE_SHADOW_BITS 1585U"))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported", "semantic_dut_inventory").foreach { flag =>
      assert((m \ flag) == JBool(false) && (d \ flag) == JBool(false))
    }
    assert((m \ "native_control_bank_state_owner") == JString("unsupported_live_response_registers"))
    Seq("", "1bad", "bad\n#define X 1", "bad-name").foreach(p =>
      intercept[IllegalArgumentException](b.header(p)))
    save("manifest.json", b.manifest); save("binding.json", b.binding)
    save("constants.h", h); save("manifest.inc", b.cppManifest)
  }

  it should "preserve independently validated physical CE descriptors and reject a different gate count" in {
    val in = getClass.getResourceAsStream("/checkpoint/NativeStorage.firrtl")
    val text = try new String(in.readAllBytes(), UTF_8) finally in.close()
    val reference = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(text), ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val deps = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, deps)
    val physical = CheckpointCaptureDescriptors.fromNativeGenerated(banks, reference, manifest, plan, deps)
    assert(physical.physicalClockGates == 2)
    val b = CheckpointNativeCaptureDescriptors.bind(physical, 130, 2, owners)
    assert(b.entries.take(physical.entries.size) == physical.entries)
    assert(b.entries.take(physical.entries.size).takeRight(2).map(_.bank) == Seq(255, 255))
    intercept[IllegalArgumentException](CheckpointNativeCaptureDescriptors.bind(physical, 130, 3, owners))
  }

  it should "elaborate the original ROM internally without externally replaceable descriptor inputs" in {
    val b = bound(); val dir = out.resolve("bound-endpoint")
    val rtl = new ChiselStage().emitVerilog(new CheckpointNativeBoundWidgetEndpoint(b),
      Array("--target-dir", dir.toString))
    val circuit = Parser.parse(Files.readString(dir.resolve("CheckpointNativeBoundWidgetEndpoint.fir")))
    val top = circuit.modules.find(_.name == "CheckpointNativeBoundWidgetEndpoint").get.asInstanceOf[FirrtlModule]
    assert(!top.ports.exists(p => p.name.matches("io_descriptor(Valid|Bank|Slot|Width|Depth)")))
    val native = circuit.modules.collectFirst { case e: ExtModule if e.defname == "CheckpointNativeCaptureTransport" => e }.get
    val params = native.params.collect { case IntParam(n, v) => n -> v }.toMap
    assert(params("ENTRIES") == 1 && params("MANIFEST_SHA256") == BigInt(b.manifestSha256, 16))
    assert(params("CAPTURE_NATIVE_CONTROL") == 1 && params("SNAPSHOT_BANK") == 252 && params("NATIVE_CONTROL_BANK") == 251)
    assert(circuit.modules.exists(_.name == "CheckpointCaptureDescriptorROM"))
    assert(rtl.contains("originalROM") && rtl.contains("io_descriptorEntry"))
    save("endpoint-observation.json", b.endpoint.observationBinding)
    println(s"NATIVE_BOUND_ENDPOINT_ELABORATED out=$out original=1 stream=4 bits=130 gates=3 owners=2")
  }
}
