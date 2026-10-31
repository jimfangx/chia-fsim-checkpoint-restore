// See LICENSE for license details.
package midas.checkpoint

import chisel3.RawModule
import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.stage.GoldenGateCompilerPhase
import midas.widgets._
import org.json4s._
import org.scalatest.flatspec.AnyFlatSpec

/** Exercise actual elaborated transports at the production image boundary.
  * A valid stream manifest is deliberately insufficient: transport/clock/host
  * self-state has no complete atomic owner yet. Direct component elaboration
  * stays available to synthesis and VCS regression probes.
  */
class NativeCheckpointImageGuardSpec extends AnyFlatSpec {
  private val out = Paths.get(sys.props("checkpoint.widget.out")).resolve("native-image-guard")
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = Files.write(out.resolve(name), text.getBytes(UTF_8))
  private def lower(text: String): CircuitState = new Compiler(Forms.LowForm)
    .execute(CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def elaborate(name: String)(module: => RawModule): CircuitState = {
    val dir = out.resolve(name)
    new ChiselStage().emitVerilog(module, Array("--target-dir", dir.toString))
    // Use the actual emitted FIRRTL, with EVERY annotation/header removed.
    lower(Files.readString(dir.resolve(name + ".fir"))).copy(annotations = Nil)
  }
  private lazy val raw = elaborate("CheckpointNativeWidgetEndpoint") {
    new CheckpointNativeWidgetEndpoint(CheckpointNativeEndpointParams(130, 3, 2, 1, "1234567890abcdef" * 4))
  }
  private lazy val bound = {
    val reference = lower("""circuit GuardFixture :
      |  module GuardFixture :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output value : UInt<64>
      |    reg state : UInt<64>, clock with :
      |      reset => (reset, UInt<64>(0))
      |    state <= add(state, UInt<1>(1))
      |    value <= state
      |""".stripMargin)
    val inventory = SemanticStateInventory.discover(reference, reference.circuit.main)
    val banks = GeneratedStateBankAccess.generate(reference, inventory)
    val original = CheckpointCaptureDescriptors.fromGenerated(banks, inventory)
    val descriptors = CheckpointNativeCaptureDescriptors.bind(original, 130, 3,
      Seq("fixture.functional", "fixture.host"))
    save("bound-manifest.json", descriptors.manifest)
    save("bound-binding.json", descriptors.binding)
    save("bound-metadata.h", descriptors.header("GUARD"))
    elaborate("CheckpointNativeBoundWidgetEndpoint")(new CheckpointNativeBoundWidgetEndpoint(descriptors))
  }
  private def native(s: CircuitState): ExtModule = s.circuit.modules.collectFirst {
    case e: ExtModule if e.defname == "CheckpointNativeCaptureTransport" => e
  }.get
  private def replace(s: CircuitState, replacement: DefModule): CircuitState = {
    val old = native(s)
    s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case m if m.name == old.name => replacement
      case m: Module =>
        def walk(stmt: Statement): Statement = stmt.mapStmt(walk) match {
          case i: WDefInstance if i.module == old.name =>
            i.copy(module = replacement.name, tpe = firrtl.Utils.module_type(replacement))
          case other => other
        }
        m.copy(body = walk(m.body))
      case m => m
    }))
  }
  private def reject(label: String, s: CircuitState): Unit = {
    assert(s.annotations.isEmpty)
    val direct = intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(s))
    val compiler = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(s))
    Seq(direct, compiler).foreach { e =>
      assert(e.getMessage.contains("native transport requires complete atomic capture/restore owners"))
    }
    val text = s.circuit.serialize
    save(label + ".fir", text)
    save(label + ".json", SemanticStateInventory.canonical(JObject(List(
      "case" -> JString(label), "source_sha256" -> JString(SemanticStateInventory.sha256(text)),
      "annotations_present" -> JBool(false), "direct_guard_error" -> JString(direct.getMessage),
      "production_boundary_error" -> JString(compiler.getMessage),
      "image_emitted" -> JBool(false), "physical_fpga_capture" -> JBool(false)))))
  }

  behavior.of("Native transport production image admission")

  it should "reject both actual native endpoints despite valid bound descriptors and stripped metadata" in {
    reject("raw-endpoint", raw)
    reject("bound-endpoint", bound)
    assert(bound.circuit.modules.exists(_.name == "CheckpointCaptureDescriptorROM"))
    assert(native(bound).params.collectFirst { case IntParam("MANIFEST_SHA256", v) => v }
      .exists(v => v != 0 && v != BigInt(NativeCheckpointWidgetReservation.digest, 16)))
  }

  it should "reject known native definitions independently of flags, parameter validity or ABI damage" in {
    val e = native(bound)
    val variants = Seq(
      "no-parameters" -> e.copy(params = Nil),
      "local-clocks-no-control" -> e.copy(params = e.params.map {
        case IntParam("EXTERNAL_CLOCK_NETWORK", _) => IntParam("EXTERNAL_CLOCK_NETWORK", 0)
        case IntParam("CAPTURE_NATIVE_CONTROL", _) => IntParam("CAPTURE_NATIVE_CONTROL", 0)
        case p => p
      }),
      "malformed-parameters" -> e.copy(params = Seq(StringParam("EXTERNAL_CLOCK_NETWORK", StringLit("owner")))),
      "duplicate-parameters" -> e.copy(params = e.params :+ IntParam("EXTERNAL_CLOCK_NETWORK", 0)),
      "damaged-abi" -> e.copy(ports = e.ports.filterNot(_.name == "native_control_bank_state"), params = Nil))
    variants.foreach { case (label, changed) => reject(label, replace(bound, changed)) }
  }

  it should "recognize renamed native state and parameter structures without trusting legal widths" in {
    val e = native(raw)
    val renamed = e.copy(name = "RenamedTransport", defname = "RenamedExternal")
    reject("renamed-definition", replace(raw, renamed))
    reject("renamed-ports-only", replace(raw, renamed.copy(params = Nil)))
    reject("renamed-parameter-signature", replace(raw, renamed.copy(ports =
      renamed.ports.filterNot(_.name == "native_control_bank_state"))))
    reject("renamed-malformed-width", replace(raw, renamed.copy(params = Nil, ports =
      renamed.ports.map {
        case p if p.name == "native_cut_cycle" => p.copy(direction = Input, tpe = UIntType(IntWidth(1)))
        case p => p
      })))
    reject("renamed-malformed-param-values", replace(raw, renamed.copy(
      params = renamed.params.map(p => StringParam(p.name, StringLit("unsupported"))),
      ports = renamed.ports.filterNot(_.name == "native_control_bank_state"))))
  }

  it should "recognize native state in a linked ordinary module without external parameters" in {
    val e = native(bound)
    // Recognition must not depend on opaque externals surviving linking.
    // This body is intentionally unowned, not a fabricated working adapter.
    reject("linked-native-body", replace(bound, Module(NoInfo, "LinkedNative", e.ports, EmptyStmt)))
  }

  it should "preserve ordinary compilation and component observation without granting native image admission" in {
    val plain = lower("""circuit Ordinary :
      |  module Ordinary :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input reader_idle : UInt<1>
      |    output functional_hold : UInt<1>
      |    functional_hold <= reader_idle
      |""".stripMargin)
    NativeCheckpointWidgetReservation.requireBound(plain)
    assert(GoldenGateCompilerPhase.prepareCheckpointBoundary(plain).circuit == plain.circuit)
    // A generic transport may share BITS/GATES and MMIO/DMA ports, without
    // being this native state owner. The two bounded signatures must not
    // degenerate into a match on arbitrary common transport names.
    val e = native(raw)
    val generic = e.copy(name = "GenericTransport", defname = "GenericExternal",
      params = Seq(IntParam("BITS", 130), IntParam("GATES", 3)),
      ports = e.ports.filterNot(p => Set("live_state", "snapshot_state", "native_valid",
        "native_failed", "native_cut_cycle", "native_control_bank_state")(p.name)))
    val genericState = replace(raw, generic)
    NativeCheckpointWidgetReservation.requireBound(genericState)
    GoldenGateCompilerPhase.prepareCheckpointBoundary(genericState)
    save("positive-controls.json", SemanticStateInventory.canonical(JObject(List(
      "ordinary_compiler_boundary" -> JBool(true), "generic_transport_boundary" -> JBool(true),
      "direct_component_elaboration" -> JBool(true), "native_image_admitted" -> JBool(false)))))
  }
}
