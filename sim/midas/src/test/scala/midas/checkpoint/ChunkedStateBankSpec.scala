// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.annotations.{MemoryFileInlineAnnotation, ReferenceTarget}
import midas.widgets.CheckpointCaptureDescriptors
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class ChunkedStateBankSpec extends AnyFlatSpec {
  private val source = """circuit ChunkedStorage :
  module ChunkedStorage :
    input clock : Clock
    input reset : UInt<1>
    input addr : UInt<17>
    input data : UInt<129>
    input wen : UInt<1>
    output narrowOut : UInt<9>
    output wideOut : UInt<129>
    output signedOut : SInt<65>
    output exactOut : UInt<128>
    output vectorOut : UInt<512>
    reg signedReg : SInt<65>, clock with :
      reset => (reset, SInt<65>(-1))
    reg exactReg : UInt<128>, clock with :
      reset => (reset, UInt<128>(0))
    reg vectorReg : UInt<512>, clock with :
      reset => (reset, UInt<512>(0))
    signedReg <= asSInt(bits(data, 64, 0))
    exactReg <= bits(data, 127, 0)
    vectorReg <= cat(bits(data, 127, 0), cat(bits(data, 127, 0), cat(bits(data, 127, 0), bits(data, 127, 0))))
    signedOut <= signedReg
    exactOut <= exactReg
    vectorOut <= vectorReg
    mem deepMem :
      data-type => UInt<9>
      depth => 65537
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    mem wideMem :
      data-type => UInt<129>
      depth => 5
      reader => r
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    deepMem.r.clk <= clock
    deepMem.r.en <= UInt<1>(1)
    deepMem.r.addr <= addr
    deepMem.w.clk <= clock
    deepMem.w.en <= wen
    deepMem.w.addr <= addr
    deepMem.w.mask <= UInt<1>(1)
    deepMem.w.data <= bits(data, 8, 0)
    narrowOut <= deepMem.r.data
    wideMem.r.clk <= clock
    wideMem.r.en <= UInt<1>(1)
    wideMem.r.addr <= bits(addr, 2, 0)
    wideMem.w.clk <= clock
    wideMem.w.en <= wen
    wideMem.w.addr <= bits(addr, 2, 0)
    wideMem.w.mask <= UInt<1>(1)
    wideMem.w.data <= data
    wideOut <= wideMem.r.data
"""
  private def lower(text: String) = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = {
    val result = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(state.copy(annotations = state.annotations :+
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    result.annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
  }
  behavior.of("Chunked generated storage")

  it should "bind logical signed, vector, non-power-of-two wide RAM and deep RAM without changing StateIDs" in {
    val state = lower(source).copy(annotations = Seq("deepMem" -> "deep.hex", "wideMem" -> "wide.hex").map {
      case (name, file) => MemoryFileInlineAnnotation(ReferenceTarget("ChunkedStorage", "ChunkedStorage", Nil, name, Nil), file)
    })
    val manifest = SemanticStateInventory.discover(state, state.circuit.main)
    val result = GeneratedStateBankAccess.generate(state, manifest)
    val descriptors = CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 5 && descriptors.entries == result.entries)
    val doc = parse(result.binding)
    assert((doc \ "protocol") == JString("latched-banked64-chunked-v2"))
    val rows = (doc \ "state").asInstanceOf[JArray].arr
    val expected = Map("signedReg" -> (65, 1, 2), "exactReg" -> (128, 1, 2), "vectorReg" -> (512, 1, 8),
      "wideMem" -> (129, 5, 15), "deepMem" -> (9, 65537, 65537))
    rows.foreach { r =>
      val name = (r \ "hierarchy").values.toString.stripPrefix("ChunkedStorage.")
      val (w, d, words) = expected(name)
      assert((r \ "width") == JInt(w) && (r \ "depth") == JInt(d) && (r \ "transfer_depth") == JInt(words))
    }
    def reject(key: String, value: JValue): Unit = {
      val badRows = rows.updated(0, rows.head.asInstanceOf[JObject].transformField { case (`key`, _) => (key, value) })
      val bad = doc.asInstanceOf[JObject].transformField { case ("state", _) => ("state", JArray(badRows)) }
      intercept[IllegalArgumentException] {
        CheckpointCaptureDescriptors.fromGenerated(result.copy(binding = SemanticStateInventory.canonical(bad)), manifest)
      }
    }
    reject("chunks", JInt(999)); reject("transfer_width", JInt(1)); reject("transfer_depth", JInt(1))
    val simManifest = SimulatorStateInventory.discover(state)
    val sim = GeneratedStateBankAccess.generate(state, simManifest)
    assert(CheckpointCaptureDescriptors.fromGenerated(sim, simManifest).entries == sim.entries)
    assert(result.entries.map(_.stateID).toSet.intersect(sim.entries.map(_.stateID).toSet).isEmpty)
    val reference = emit(state).replace("module ChunkedStorage(", "module ReferenceChunkedStorage(")
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("chunked"); Files.createDirectories(out)
      (new chisel3.stage.ChiselStage).emitVerilog(new midas.widgets.CheckpointCaptureDescriptorROM(descriptors),
        Array("--target-dir", out.resolve("rom").toString))
      val deep = (0 until 65537).map(i => f"${(i * 37 + 11) & 511}%03x").mkString("\n") + "\n"
      val wide = (0 until 5).map(i => ((BigInt(1) << 128) | (BigInt(i + 1) << 64) | (17 + i * 29)).toString(16)).mkString("\n") + "\n"
      Seq("generated-bank-input.fir" -> state.circuit.serialize, "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> reference,
        "generated-bank-semantic.json" -> manifest, "generated-bank-binding.json" -> result.binding,
        "generated-bank-simulator.json" -> simManifest, "generated-bank-simulator-binding.json" -> sim.binding,
        "generated-bank-simulator.fir" -> sim.state.circuit.serialize,
        "descriptor-manifest.inc" -> descriptors.cppManifest, "deep.hex" -> deep, "wide.hex" -> wide
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }

  it should "reject flattened address overflow before returning hardware" in {
    val state = lower(source.replace("depth => 5", "depth => 715827883"))
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(state, SemanticStateInventory.discover(state, state.circuit.main))
    }.getMessage.contains("31-bit transfer depth"))
  }
}
