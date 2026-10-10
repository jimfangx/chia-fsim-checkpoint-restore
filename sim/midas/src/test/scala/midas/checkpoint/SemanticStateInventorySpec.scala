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

  it should "defer an explicit bridge-extracted root and consume it only at the pre-FAME boundary" in {
    val state = lower(source).copy(annotations = Seq(SemanticBridgeCheckpointRootAnnotation("Harness.dut")))
    val deferred = SemanticStateInventory(state)
    assert(deferred == state)
    val result = new BridgeSemanticStateInventory().execute(deferred)
    assert(result.circuit == state.circuit)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    assert(files(".semantic-state.json") == manifest())
    assert(files(".semantic-clean.sv").contains("module InventoryDUT("))
    assert(!result.annotations.exists {
      case _: SemanticCheckpointRootAnnotation | _: SemanticBridgeCheckpointRootAnnotation => true
      case _ => false
    })
    assert(new BridgeSemanticStateInventory().execute(result) == result)
    val ordinary = lower(source)
    assert(new BridgeSemanticStateInventory().execute(ordinary) == ordinary)
  }

  it should "reject mixed inventory phases and duplicate deferred selections before emission" in {
    val root = SemanticBridgeCheckpointRootAnnotation("Harness.dut")
    val early = SemanticCheckpointRootAnnotation("Harness.dut")
    for (annotations <- Seq(Seq(root, early), Seq(root, root))) {
      val state = lower(source).copy(annotations = annotations)
      intercept[IllegalArgumentException](SemanticStateInventory(state))
      intercept[IllegalArgumentException](new BridgeSemanticStateInventory().execute(state))
    }
    val state = lower(source).copy(annotations = Seq(root, SemanticBankedAccessAnnotation("Harness.dut", Seq.empty)))
    intercept[IllegalArgumentException](SemanticStateInventory(state))
    intercept[IllegalArgumentException](new BridgeSemanticStateInventory().execute(state))
  }

  it should "describe extracted clock aliases separately and reject generated or undriven clocks" in {
    val alias = source.replace("    output count : UInt<9>",
      "    output bridgeClock : Clock\n    output count : UInt<9>")
      .replace("    reg counter : UInt<9>", "    bridgeClock <= clock\n    reg counter : UInt<9>")
    val state = lower(alias).copy(annotations = Seq(SemanticBridgeCheckpointRootAnnotation("Harness.dut")))
    intercept[IllegalArgumentException](SemanticStateInventory.discover(state, "Harness.dut"))
    val result = new BridgeSemanticStateInventory().execute(state)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    assert(entries(files(".semantic-state.json")) == entries(manifest()))
    val boundary = parse(files(".semantic-boundary.json"))
    assert((boundary \ "boundary_version") == JInt(2))
    assert((boundary \ "clock_aliases") == JArray(List(JObject(List(
      "direction" -> JString("output"), "name" -> JString("bridgeClock"), "source" -> JString("clock"))))))
    assert(!(boundary \ "ports").asInstanceOf[JArray].arr.exists(p => (p \ "name") == JString("bridgeClock")))
    assert(files(".semantic-clean.sv").contains("bridgeClock"))
    val generated = lower(alias.replace("bridgeClock <= clock", "bridgeClock <= asClock(bits(data, 0, 0))"))
      .copy(annotations = state.annotations)
    intercept[IllegalArgumentException](new BridgeSemanticStateInventory().execute(generated))
  }

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
    assert(files(".semantic-clean.sv").contains("module InventoryDUT("))
    assert(!files(".semantic-clean.sv").contains("module Harness("))
    assert((parse(files(".semantic-boundary.json")) \ "dut_module") == JString("InventoryDUT"))
    assert((parse(files(".semantic-boundary.json")) \ "ports").asInstanceOf[JArray].arr.size == 13)

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

  it should "reject incomplete, reordered, reshaped or ambiguous banked bindings before emission" in {
    val s = lower(source)
    val root = SemanticCheckpointRootAnnotation("Harness.dut")
    val bindings = entries(manifest()).zipWithIndex.map { case (e, slot) =>
      StateAccessEntry((e \ "state_id").values.toString, 0, slot,
        (e \ "width").asInstanceOf[JInt].num.toInt, (e \ "depth").asInstanceOf[JInt].num.toInt)
    }
    def bind(es: Seq[StateAccessEntry], path: String = "Harness.dut") =
      s.copy(annotations = Seq(root, SemanticBankedAccessAnnotation(path, es)))
    val result = SemanticStateInventory(bind(bindings))
    val sidecar = result.annotations.collectFirst {
      case a: GoldenGateOutputFileAnnotation if a.fileSuffix == ".semantic-access.json" => parse(a.body)
    }.get
    assert((sidecar \ "state_manifest_sha256") == JString(SemanticStateInventory.sha256(manifest())))
    assert((sidecar \ "state").asInstanceOf[JArray].arr.size == 8)
    Seq(bindings.dropRight(1), bindings.reverse, bindings :+ bindings.head,
      bindings.updated(0, bindings.head.copy(width = 8)),
      bindings.updated(1, bindings(1).copy(slot = bindings.head.slot))).foreach { es =>
      intercept[IllegalArgumentException](SemanticStateInventory(bind(es)))
    }
    intercept[IllegalArgumentException](SemanticStateInventory(bind(bindings, "Harness.other")))
    intercept[IllegalArgumentException](SemanticStateInventory(s.copy(annotations =
      Seq(SemanticBankedAccessAnnotation("Harness.dut", bindings)))))
    intercept[IllegalArgumentException](bindings.head.copy(width = 65))
    intercept[IllegalArgumentException](bindings.head.copy(depth = 0))
  }

  it should "resolve memory clock validity guarded by that port's enable without accepting conditional clocks" in {
    val guarded = source.replace("sram.w.clk <= clock", "sram.w.clk <= validif(wen, clock)")
    assert(entries(manifest(guarded)) == entries(manifest()))
    val loweredChisel = source
      .replace("sram.w.clk <= clock", "node writeClock = validif(wen, clock)\n    sram.w.clk <= writeClock")
      .replace("sram.w.en <= wen", "node writeEnable = mux(wen, UInt<1>(1), UInt<1>(0))\n    sram.w.en <= writeEnable")
    assert(entries(manifest(loweredChisel)) == entries(manifest()))
    val unrelated = source.replace("sram.w.clk <= clock", "sram.w.clk <= validif(bits(data, 0, 0), clock)")
    val error = intercept[IllegalArgumentException](manifest(unrelated))
    assert(error.getMessage.contains("clock"), error.getMessage)
    val delayedEnable = source
      .replace("reg counter : UInt<9>, clock with :",
        "reg delayedEnable : UInt<1>, clock\n    delayedEnable <= wen\n    reg counter : UInt<9>, clock with :")
      .replace("sram.w.clk <= clock", "sram.w.clk <= validif(delayedEnable, clock)")
    // A register's next-state driver is not a combinational alias of its value.
    intercept[IllegalArgumentException](manifest(delayedEnable))
    val registerClock = source.replace("reg counter : UInt<9>, clock with :",
      "reg counter : UInt<9>, validif(wen, clock) with :")
    intercept[IllegalArgumentException](manifest(registerClock))
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
