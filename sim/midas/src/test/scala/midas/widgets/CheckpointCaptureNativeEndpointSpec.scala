// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.stage.ChiselStage
import firrtl.Parser
import firrtl.ir.{Circuit, ClockType, Connect, DefNode, Direction, DoPrim, Expression, ExtModule,
  IntParam, IntWidth, Module => FirrtlModule, Port, Reference, Statement, UIntLiteral, UIntType,
  Input => FirrtlInput, Output => FirrtlOutput}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import midas.checkpoint.CheckpointTransportControlState
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** Independent literal fixture: the real coordinator and the misleading native
  * payload contain opposite cut certificates. Also expose the inverse mapping
  * so the EDA bench can check every raw/bank bit with arbitrary input planes.
  */
class CheckpointNativeEndpointLayoutProbe extends Module {
  private val layout = CheckpointNativeEndpointLayout(130, 3)
  val io = IO(new Bundle {
    val raw = Input(UInt(layout.rawBits.W)); val bank = Input(UInt(67.W))
    val view = Output(UInt(layout.endpointBits.W)); val recoveredRaw = Output(UInt(layout.rawBits.W))
    val recoveredBank = Output(UInt(67.W)); val acknowledged = Output(Bool())
    val correctHighCertificate = Output(Bool()); val misleadingLowCertificate = Output(Bool())
  })
  val view = layout.endpointView(io.raw, io.bank)
  io.view := view; io.recoveredRaw := layout.rawComposite(view); io.recoveredBank := layout.bankState(view)
  io.acknowledged := CheckpointLiveWidgetSnapshot.acknowledgedCut(view, 42.U(64.W))
  val fields = CheckpointTransportControlState.fields(1).map(f => f.path -> f).toMap
  def certificate: BigInt = {
    val state = fields("transport.cut.state"); val cycle = fields("transport.cut.cut_cycle")
    (BigInt(2) << state.offset) | (BigInt(42) << cycle.offset)
  }
  // A larger native-owner prefix holds the entire misleading low certificate
  // independently of the actual coordinator, whose cut fields begin at 339.
  val conflicting = CheckpointNativeEndpointLayout(512, 3)
  val high = (certificate << conflicting.nativeOwnerBits).U(conflicting.rawBits.W)
  val low = certificate.U(conflicting.rawBits.W)
  io.correctHighCertificate := CheckpointLiveWidgetSnapshot.acknowledgedCut(
    conflicting.endpointView(high, 0.U(67.W)), 42.U(64.W))
  io.misleadingLowCertificate := CheckpointLiveWidgetSnapshot.acknowledgedCut(
    conflicting.endpointView(low, 0.U(67.W)), 42.U(64.W))
}

class CheckpointCaptureNativeEndpointSpec extends AnyFlatSpec {
  private val cfg = CheckpointNativeEndpointParams(130, 3, 2, 1, "1234567890abcdef" * 4)
  private val workspace = Iterator.iterate(Paths.get("").toAbsolutePath.normalize)(_.getParent)
    .takeWhile(_ != null).find(p => Files.isRegularFile(p.resolve("plan_agent.md")))
    .getOrElse(throw new IllegalArgumentException("native endpoint tests require the checkpoint workspace"))
  private val configuredOutput = Paths.get(sys.props("checkpoint.widget.out")).toAbsolutePath.normalize
  require(configuredOutput.startsWith(workspace.resolve("automations/checkpoint_loop/runs")),
    "native endpoint generated evidence must stay beneath checkpoint loop runs")
  private val root = configuredOutput.resolve("native-endpoint")
  private def source(name: String): String = {
    val in = getClass.getResourceAsStream("/" + name)
    require(in != null, "missing native endpoint source " + name)
    try new String(in.readAllBytes(), UTF_8) finally in.close()
  }
  private def emit(name: String)(module: => RawModule): (String, Circuit) = {
    val out = root.resolve(name); Files.createDirectories(out)
    val rtl = new ChiselStage().emitVerilog(module, Array("--target-dir", out.toString))
    (rtl, Parser.parse(Files.readString(out.resolve(name + ".fir"))))
  }

  behavior.of("Native transport production-facing endpoint API")

  it should "reject malformed geometry, reservation identities and aliased reserved selectors" in {
    Seq(0, -1, Int.MaxValue).foreach(bits => intercept[IllegalArgumentException](cfg.copy(bits = bits)))
    // This width fits the live view exactly but overflows the endpoint shadow
    // and staging state. Reject during configuration, before hardware allocation.
    intercept[IllegalArgumentException](cfg.copy(bits = Int.MaxValue - cfg.gates - 694))
    Seq(0, -1, 65536).foreach(gates => intercept[IllegalArgumentException](cfg.copy(gates = gates)))
    Seq(0, -1, 65535).foreach(owners => intercept[IllegalArgumentException](cfg.copy(owners = owners)))
    Seq(0, -1, 65535).foreach(entries => intercept[IllegalArgumentException](cfg.copy(entries = entries)))
    Seq(-1, 255, 256).foreach(bank => {
      intercept[IllegalArgumentException](cfg.copy(snapshotBank = bank))
      intercept[IllegalArgumentException](cfg.copy(nativeControlBank = bank))
    })
    intercept[IllegalArgumentException](cfg.copy(nativeControlBank = cfg.snapshotBank))
    Seq("0" * 64, "f" * 64, "x" * 64, "1").foreach(hash =>
      intercept[IllegalArgumentException](cfg.copy(manifestSha256 = hash)))
    assert(cfg.copy(bits = 1, captureNativeControl = false).totalEntries == 2)
    assert(cfg.copy(bits = 64).totalEntries == 3)
    assert(cfg.totalEntries == 4)
  }

  it should "instantiate the exact existing native SV ABI and actual shared NASTI endpoint" in {
    val (rtl, circuit) = emit("CheckpointNativeWidgetEndpoint")(new CheckpointNativeWidgetEndpoint(cfg))
    val transport = circuit.modules.collectFirst {
      case m: ExtModule if m.defname == "CheckpointNativeCaptureTransport" => m
    }.get
    def ground(port: Port): (String, (Direction, Int)) = port.tpe match {
      case ClockType => port.name -> (port.direction, 1)
      case UIntType(IntWidth(w)) => port.name -> (port.direction, w.toInt)
      case _ => fail("unresolved native transport port " + port.serialize)
    }
    val inputs = Seq("clock" -> 1, "reset" -> 1, "ce" -> 3, "target_cycle" -> 64,
      "owner_present" -> 2, "owner_frozen" -> 2, "owner_error" -> 2, "reader_idle" -> 1,
      "live_state" -> 130, "mmio_valid" -> 1, "mmio_write" -> 1, "mmio_address" -> 8,
      "mmio_wdata" -> 32, "mmio_response_ready" -> 1, "descriptor_valid" -> 1,
      "descriptor_bank" -> 8, "descriptor_slot" -> 16, "descriptor_width" -> 16,
      "descriptor_depth" -> 32, "bank_request_ready" -> 1, "bank_response_valid" -> 1,
      "bank_response_data" -> 64, "bank_response_error" -> 1, "dma_ready" -> 1)
    val outputs = Seq("root_clock" -> 1, "gate_clock" -> 3, "clock_mask" -> 1,
      "freeze" -> 1, "functional_hold" -> 1, "native_valid" -> 1, "native_failed" -> 1,
      "native_cut_cycle" -> 64, "captured_ce" -> 3, "snapshot_state" -> 130,
      "mmio_ready" -> 1, "mmio_response_valid" -> 1, "mmio_rdata" -> 32,
      "mmio_response_error" -> 1, "descriptor_entry" -> 32, "bank_request_valid" -> 1,
      "bank" -> 8, "slot" -> 16, "address" -> 32, "bank_response_ready" -> 1,
      "dma_valid" -> 1, "dma_data" -> 512, "dma_last" -> 1, "dma_abort" -> 1,
      "control_state" -> 760, "native_control_bank_state" -> 67)
    val expected = (inputs.map { case (n, w) => n -> (FirrtlInput, w) } ++
      outputs.map { case (n, w) => n -> (FirrtlOutput, w) }).toMap
    assert(transport.ports.map(ground).toMap == expected && transport.ports.size == expected.size)
    assert(transport.params.collect { case IntParam(n, v) => n -> v }.toMap == Map(
      "BITS" -> BigInt(130), "GATES" -> BigInt(3), "OWNERS" -> BigInt(2), "ENTRIES" -> BigInt(1),
      "EXTERNAL_CLOCK_NETWORK" -> BigInt(1), "CAPTURE_NATIVE_CONTROL" -> BigInt(1),
      "SNAPSHOT_BANK" -> BigInt(252), "NATIVE_CONTROL_BANK" -> BigInt(251),
      "MANIFEST_SHA256" -> BigInt(cfg.manifestSha256, 16)))
    assert(circuit.modules.exists(_.name == "CheckpointNastiAdapter"))
    assert(circuit.modules.exists(_.name == "CheckpointLiveWidgetSnapshot"))
    assert(rtl.contains("io_native_functionalHold") && rtl.contains("io_native_nativeControlBankState"))
    val snapshot = circuit.modules.find(_.name == "CheckpointLiveWidgetSnapshot").get.asInstanceOf[FirrtlModule]
    assert(snapshot.body.serialize.contains("saved : UInt<1585>"))
    assert(!transport.ports.exists(p => p.name.startsWith("restore_") || p.name.startsWith("physical_")))
    val out = root.resolve("CheckpointNativeWidgetEndpoint")
    CheckpointNativeCaptureTransportBlackBox.resources.foreach { name =>
      assert(Files.readString(out.resolve(name)) == source(name))
    }
    Files.write(out.resolve("native-endpoint-observation.json"), cfg.observationBinding.getBytes(UTF_8))
    val binding = parse(cfg.observationBinding)
    val expectedGeometry = Seq("original_bits" -> 130, "gates" -> 3, "owners" -> 2,
      "external_clock_network" -> 1, "endpoint_shadow_layout_version" -> 2, "endpoint_shadow_bits" -> 1585)
    expectedGeometry.foreach { case (key, value) => assert((binding \ key) == JInt(value)) }
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
    assert((binding \ "native_control_bank_state_owner") == JString("unsupported_live_response_registers"))
    assert((binding \ "clock_ownership") == JString("externally_owned_clock_network_required"))
    assert((binding \ "root_and_gate_clock_outputs") == JString("constant_zero_in_external_clock_network_mode"))
    assert((binding \ "endpoint_shadow_restore_supported") == JBool(false))
    println(s"NATIVE_ENDPOINT_ELABORATED out=$out top=CheckpointNativeWidgetEndpoint bits=130 gates=3 owners=2 entries=1 shadow=1585")
  }

  it should "preserve all native and bank bits while using the real high coordinator certificate" in {
    val (rtl, circuit) = emit("CheckpointNativeEndpointLayoutProbe")(new CheckpointNativeEndpointLayoutProbe)
    // Evaluate the emitted FIRRTL expression graph, including actual named
    // intermediate nodes. Optimizer wire elimination is not part of the ABI.
    val module = circuit.modules.find(_.name == "CheckpointNativeEndpointLayoutProbe").get.asInstanceOf[FirrtlModule]
    val nodes = scala.collection.mutable.Map.empty[String, Expression]
    val outputs = scala.collection.mutable.Map.empty[String, Expression]
    def collect(statement: Statement): Unit = {
      statement match {
        case n: DefNode => nodes(n.name) = n.value
        case c: Connect => outputs(c.loc.serialize) = c.expr
        case _ => ()
      }
      statement.foreachStmt(collect)
    }
    collect(module.body)
    def expressionWidth(expression: Expression): Int = expression.serialize match {
      case "io.raw" => 760
      case "io.bank" => 67
      case _ => expression match {
        case r: Reference if nodes.contains(r.name) => expressionWidth(nodes(r.name))
        case UIntLiteral(_, IntWidth(width)) => width.toInt
        case DoPrim(firrtl.PrimOps.Cat, Seq(high, low), _, _) => expressionWidth(high) + expressionWidth(low)
        case DoPrim(firrtl.PrimOps.Bits, Seq(_), Seq(high, low), _) => (high - low + 1).toInt
        case other => fail("unexpected native layout width: " + other.serialize)
      }
    }
    def evaluate(expression: Expression, inputs: Map[String, BigInt]): BigInt =
      inputs.getOrElse(expression.serialize, expression match {
        case r: Reference if nodes.contains(r.name) => evaluate(nodes(r.name), inputs)
        case UIntLiteral(value, _) => value
        case DoPrim(firrtl.PrimOps.Cat, Seq(high, low), _, _) =>
          (evaluate(high, inputs) << expressionWidth(low)) | evaluate(low, inputs)
        case DoPrim(firrtl.PrimOps.Bits, Seq(value), Seq(high, low), _) =>
          (evaluate(value, inputs) >> low.toInt) & ((BigInt(1) << (high - low + 1).toInt) - 1)
        case other => fail("unexpected native layout expression: " + other.serialize)
      })
    for (plane <- 0 until 827) {
      val raw = if (plane < 760) BigInt(1) << plane else BigInt(0)
      val bank = if (plane >= 760) BigInt(1) << (plane - 760) else BigInt(0)
      val inputs = Map("io.raw" -> raw, "io.bank" -> bank)
      val expectedView = ((raw >> 283) & ((BigInt(1) << 411) - 1)) |
        ((raw & ((BigInt(1) << 283) - 1)) << 411) | ((raw >> 694) << 694) | (bank << 760)
      assert(evaluate(outputs("io.view"), inputs) == expectedView, s"view plane $plane")
      assert(evaluate(outputs("io.recoveredRaw"), inputs) == raw, s"inverse raw plane $plane")
      assert(evaluate(outputs("io.recoveredBank"), inputs) == bank, s"inverse bank plane $plane")
    }
    assert(rtl.contains("assign io_correctHighCertificate = 1'h1;"))
    assert(rtl.contains("assign io_misleadingLowCertificate = 1'h0;"))
    assert(rtl.contains("io_raw[693:283]"), "endpoint low bits must be the actual native coordinator")
    val layout = cfg.layout
    assert(layout.nativeOwnerBits == 283 && layout.rawBits == 760 && layout.endpointBits == 827)
  }

  it should "leave ordinary and physical production shadow headers at their existing layouts" in {
    val ordinary = CheckpointCaptureWidgetParams(3, cfg.manifestSha256, Seq("owner"))
    val physical = ordinary.copy(physicalClockGates = 2)
    val ordinaryHeader = CheckpointCaptureABI.header("ORDINARY", 0, ordinary, 0)
    val physicalHeader = CheckpointCaptureABI.header("PHYSICAL", 0, physical, 0)
    assert(ordinaryHeader.contains("SHADOW_CONTROL_BITS 411U") && ordinaryHeader.contains("SHADOW_BITS 1169U"))
    assert(physicalHeader.contains("SHADOW_CONTROL_BITS 483U") && physicalHeader.contains("SHADOW_BITS 1241U"))
    assert(!ordinaryHeader.contains("CheckpointNativeCaptureTransport") && !physicalHeader.contains("CheckpointNativeCaptureTransport"))
  }
}
