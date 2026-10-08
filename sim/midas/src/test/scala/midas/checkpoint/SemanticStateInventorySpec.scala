// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser, EmitCircuitAnnotation}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class SemanticStateInventorySpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val source = try resource.mkString finally resource.close()
  private def lower(src: String): CircuitState =
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(src), ChirrtlForm)).copy(form = LowForm)
  private def manifest(src: String = source): String = SemanticStateInventory.discover(lower(src), "Harness.dut")
  private def entries(text: String): List[JValue] = (parse(text) \ "state").asInstanceOf[JArray].arr

  behavior.of("Pre-FAME semantic inventory")

  it should "inventory every register, FSM bit vector, array word and SRAM in two same-module instances" in {
    val state = lower(source)
    val result = SemanticStateInventory(state.copy(annotations = Seq(SemanticCheckpointRootAnnotation("Harness.dut"))))
    assert(result.circuit == state.circuit)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    val text = files(".semantic-state.json")
    val es = entries(text)
    assert(es.size == 8)
    val paths = es.map(e => (e \ "hierarchy").values.toString).toSet
    assert(paths == Set("counter", "fsm", "bank_0", "bank_1", "bank_2", "sram", "a.saved", "b.saved")
      .map("Harness.dut." + _))
    assert(es.map(e => (e \ "state_id").values).distinct.size == 8)
    assert(es.forall(e => (e \ "clock_domain") == JString("Harness.dut.clock")))
    val ram = es.find(e => (e \ "kind") == JString("memory")).get
    assert((ram \ "width") == JInt(9) && (ram \ "depth") == JInt(8))
    assert(files(".semantic-reference.fir") == state.circuit.serialize)
    assert((parse(text) \ "reference_sha256") == JString(SemanticStateInventory.sha256(state.circuit.serialize)))

    // Real compiler artifacts for cross-language codec and ordinary clean RTL VCS tests.
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val output = Paths.get(dir)
      Files.createDirectories(output)
      files.foreach { case (suffix, body) => Files.write(output.resolve("InventoryDUT" + suffix), body.getBytes(UTF_8)) }
      val reference = state.circuit.copy(main = "InventoryDUT", modules = state.circuit.modules.filter(_.name != "Harness"))
      val clean = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
        CircuitState(reference, LowForm, Seq(EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      )
      Files.write(output.resolve("InventoryDUT.sv"), clean.getEmittedCircuit.value.getBytes(UTF_8))
    }
  }

  it should "be deterministic and keep IDs stable across declaration reordering and unrelated harness state" in {
    val first = entries(manifest())
    assert(manifest() == manifest())
    val reordered = source.replace("inst a of Leaf\n    inst b of Leaf", "inst b of Leaf\n    inst a of Leaf")
      .replace("harnessOnly", "unrelatedHarnessState")
    assert(entries(manifest(reordered)) == first)
  }

  it should "require explicit selection and leave opt-out compilation untouched" in {
    val s = lower(source)
    assert(SemanticStateInventory(s) == s)
    intercept[IllegalArgumentException](SemanticStateInventory.discover(s, "Harness.absent"))
    intercept[IllegalArgumentException](SemanticStateInventory(s.copy(annotations = Seq(
      SemanticCheckpointRootAnnotation("Harness.dut"), SemanticCheckpointRootAnnotation("Harness.dut")))))
    intercept[IllegalArgumentException](SemanticStateInventory.discover(s.copy(form = ChirrtlForm), "Harness.dut"))
  }

  it should "reject opaque state, async reset, derived clocks, multiple clocks, and hidden memory pipelines" in {
    val opaque = source.replace("module Leaf :", "extmodule Leaf :")
      .replace("    reg saved : UInt<9>, clock with :\n      reset => (reset, UInt<9>(0))\n    saved <= data\n    value <= saved", "    defname = OpaqueLeaf")
    val negatives = Seq(
      opaque -> "opaque external module",
      source.replace("input reset : UInt<1>", "input reset : AsyncReset") -> "reset",
      source.replace("a.clock <= clock", "a.clock <= asClock(bits(data, 0, 0))") -> "clock",
      source.replace("    input wen : UInt<1>", "    input wen : UInt<1>\n    input otherClock : Clock")
        .replace("b.clock <= clock", "b.clock <= otherClock")
        .replace("dut.wen <= wen", "dut.wen <= wen\n    dut.otherClock <= otherClock") -> "multiple target clock domains",
      source.replace("read-latency => 0", "read-latency => 1") -> "memory latency",
    )
    negatives.foreach { case (src, message) =>
      val error = intercept[IllegalArgumentException](manifest(src))
      assert(error.getMessage.contains(message), error.getMessage)
    }
  }
}
