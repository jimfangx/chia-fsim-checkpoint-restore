// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser, EmitCircuitAnnotation}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.annotations.{ReferenceTarget, ModuleName, CircuitName}
import firrtl.transforms.{DontTouchAnnotation, BlackBoxInlineAnno}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class SemanticStateInventorySpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val source = try resource.mkString finally resource.close()
  private def lower(src: String, injectClockLoop: Boolean = false): CircuitState = {
    // Only adversarial unit cases bypass the earlier FIRRTL loop checker to
    // exercise our own cycle rejection on an already resolved LowForm graph.
    val annotations = if (injectClockLoop) Seq(firrtl.transforms.DontCheckCombLoopsAnnotation) else Seq.empty
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(src), ChirrtlForm, annotations)).copy(form = LowForm)
  }
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

  private val plusargSource = """circuit Top :
  module Top :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<32>
    output result : UInt<32>
    output repeated : UInt<32>
    output flag : UInt<1>
    output wide : UInt<65>
    output count : UInt<32>
    reg counter : UInt<32>, clock with :
      reset => (reset, UInt<32>(0))
    counter <= add(counter, data)
    inst a of Reader32
    inst b of Reader1
    inst w of Reader65
    inst relay of Relay
    result <= add(counter, a.out)
    repeated <= add(counter, relay.value)
    flag <= b.out
    wide <= w.out
    count <= counter
  module Relay :
    output value : UInt<32>
    inst a of Reader32
    value <= a.out
  extmodule Reader32 :
    output out : UInt<32>
    defname = plusarg_reader
    parameter WIDTH = 32
    parameter DEFAULT = 7
    parameter FORMAT = "semantic_limit=%d"
  extmodule Reader1 :
    output out : UInt<1>
    defname = plusarg_reader
    parameter WIDTH = 1
    parameter DEFAULT = 0
    parameter FORMAT = "semantic_flag=%b"
  extmodule Reader65 :
    output out : UInt<65>
    defname = plusarg_reader
    parameter WIDTH = 65
    parameter DEFAULT = 9
    parameter FORMAT = "semantic_wide=%h"
"""
  private def plusargState(src: String = plusargSource, changeModel: Boolean = false): CircuitState = {
    val resource = Source.fromResource("vsrc/plusarg_reader.v")
    val model = try resource.mkString finally resource.close()
    val state = lower(src)
    state.copy(annotations = state.annotations ++ Seq("Reader32", "Reader1", "Reader65").map { name =>
      BlackBoxInlineAnno(ModuleName(name, CircuitName("Top")), "plusarg_reader.v", model + (if (changeModel) "\n" else ""))
    } :+ SemanticCheckpointRootAnnotation("Top"))
  }

  it should "own every exact initialized external value separately from target clock state and emit its original model" in {
    val state = plusargState()
    val result = SemanticStateInventory(state)
    assert(result.circuit == state.circuit)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    val doc = parse(files(".semantic-state.json"))
    assert((doc \ "manifest_version") == JInt(2))
    assert((doc \ "clock_domain") == JString("Top.clock"))
    val es = entries(files(".semantic-state.json"))
    val initialized = es.filter(e => (e \ "clock_domain") == JString("initial"))
    assert(es.size == 5 && initialized.size == 4)
    assert(initialized.map(e => (e \ "hierarchy").values).toSet ==
      Set("Top.a.myplus", "Top.b.myplus", "Top.w.myplus", "Top.relay.a.myplus"))
    assert(initialized.forall(e => (e \ "kind") == JString("register") && (e \ "reset") == JString("none")))
    val owners = (doc \ "external_models").asInstanceOf[JArray].arr
    assert(owners.size == 4 && owners.forall(e => (e \ "adapter") == JString("semantic-plusarg-v1")))
    assert(owners.find(e => (e \ "hierarchy") == JString("Top.a")).get \ "default" == JInt(7))
    val model = state.annotations.collect { case a: BlackBoxInlineAnno => a.text }.head
    assert(files(".semantic-clean.sv").sliding(model.length).count(_ == model) == 1)
    assert(files(".semantic-clean.sv").contains(".WIDTH(65)"))
    assert(files(".semantic-reference.fir") == state.circuit.serialize)
    val changed = parse(SemanticStateInventory.discover(plusargState(plusargSource.replace("DEFAULT = 7", "DEFAULT = 8")), "Top"))
    assert((changed \ "external_models") != (doc \ "external_models"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val output = Paths.get(dir)
      Files.createDirectories(output)
      files.foreach { case (suffix, body) => Files.write(output.resolve("semantic-plusarg" + suffix), body.getBytes(UTF_8)) }
    }
  }

  it should "reject unbound, altered, duplicated and unsupported external models before semantic publication" in {
    val state = plusargState()
    val missing = state.copy(annotations = state.annotations.filterNot(_.isInstanceOf[BlackBoxInlineAnno]))
    val duplicate = state.copy(annotations = state.annotations ++ state.annotations.collect { case a: BlackBoxInlineAnno => a })
    val wrongCircuit = state.copy(annotations = state.annotations.map {
      case a: BlackBoxInlineAnno => a.copy(target = ModuleName(a.target.name, CircuitName("Other")))
      case a => a
    })
    val cases = Seq(missing, duplicate, wrongCircuit, plusargState(changeModel = true)) ++
      Seq(plusargSource.replace("WIDTH = 32", "WIDTH = 31"),
          plusargSource.replace("DEFAULT = 7", "DEFAULT = -1"),
          plusargSource.replace("DEFAULT = 0", "DEFAULT = 2"),
          plusargSource.replace("FORMAT =", "OTHER ="),
          plusargSource.replace("semantic_limit=%d", "semantic_limit=%s"),
          plusargSource.replace("defname = plusarg_reader", "defname = OtherModel"))
        .map(src => plusargState(src))
    cases.foreach { s =>
      val error = intercept[IllegalArgumentException](SemanticStateInventory(s))
      assert(error.getMessage.contains("opaque external module"), error.getMessage)
      assert(!s.annotations.exists(_.isInstanceOf[GoldenGateOutputFileAnnotation]))
    }
    // An initialized value is never a clock-domain exception for ordinary registers.
    val twoClocks = plusargSource.replace("    input reset :", "    input secondClock : Clock\n    input reset :")
      .replace("    reg counter :", "    reg other : UInt<1>, secondClock\n    other <= UInt<1>(0)\n    reg counter :")
    assert(intercept[IllegalArgumentException](SemanticStateInventory(plusargState(twoClocks)))
      .getMessage.contains("multiple target clock domains"))
  }

  it should "retain exact parent clock pin fanout beside initialized external owners in ordinary RTL" in {
    val src = plusargSource.replace("reg counter : UInt<32>, clock", "reg counter : UInt<32>, relay.clock")
      .replace("    inst relay of Relay\n", "")
      .replace("    reg counter :", "    inst relay of Relay\n    relay.clock <= clock\n    reg counter :")
      .replace("  module Relay :", "  module Relay :\n    input clock : Clock")
    val state = plusargState(src)
    val selected = state.copy(annotations = state.annotations.filterNot(_.isInstanceOf[SemanticCheckpointRootAnnotation]) :+
      SemanticBridgeCheckpointRootAnnotation("Top"))
    val result = new BridgeSemanticStateInventory().execute(selected)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    val clean = files(".semantic-clean.sv")
    assert(clean.contains("always @(posedge relay_clock)") && clean.contains(".clock(relay_clock)"))
    assert(entries(files(".semantic-state.json")) == entries(SemanticStateInventory.discover(plusargState(), "Top")))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val output = Paths.get(dir)
      Files.createDirectories(output)
      files.foreach { case (suffix, body) =>
        Files.write(output.resolve("semantic-plusarg-clock-pin" + suffix), body.getBytes(UTF_8))
      }
    }
  }

  it should "preserve extracted data inputs through optimization without changing the semantic reference" in {
    val src = source.replace("    output count : UInt<9>",
      "    input responseStatus : UInt<2>\n    input responseUser : UInt<1>\n    output count : UInt<9>")
      .replace("    dut.clock <= clock", "    dut.responseStatus <= UInt<2>(0)\n    dut.responseUser <= UInt<1>(0)\n    dut.clock <= clock")
    val state = lower(src).copy(annotations = Seq(SemanticBridgeCheckpointRootAnnotation("Harness.dut")))
    val result = new BridgeSemanticStateInventory().execute(state)
    assert(result.circuit == state.circuit)
    val selected = state.circuit.modules.find(_.name == "InventoryDUT").get
    val expected = selected.ports.filter(p => p.direction == firrtl.ir.Input && p.tpe != firrtl.ir.ClockType)
      .map(p => ReferenceTarget(state.circuit.main, selected.name, Nil, p.name, Nil)).toSet
    assert(result.annotations.collect { case DontTouchAnnotation(t) => t }.toSet == expected)
    val optimized = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      result.copy(annotations = result.annotations :+ EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
      .getEmittedCircuit.value
    assert(optimized.contains("responseStatus") && optimized.contains("responseUser"))
    val ordinary = SemanticStateInventory(state.copy(annotations = Seq(SemanticCheckpointRootAnnotation("Harness.dut"))))
    assert(!ordinary.annotations.exists(_.isInstanceOf[DontTouchAnnotation]))
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    assert(files(".semantic-reference.fir") == state.circuit.serialize)
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

  private def forwardedSource: String = source
    .replace("    output value : UInt<9>", "    output forwarded : Clock\n    output value : UInt<9>")
    .replace("    value <= saved", "    forwarded <= clock\n    value <= saved")
    .replace("    a.clock <= clock", "    a.clock <= b.forwarded")

  it should "resolve hierarchical and sibling clock forwarding without changing state membership or domains" in {
    val state = lower(forwardedSource).copy(annotations = Seq(SemanticBridgeCheckpointRootAnnotation("Harness.dut")))
    val result = new BridgeSemanticStateInventory().execute(state)
    val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
    assert(entries(files(".semantic-state.json")) == entries(manifest()))
    assert((parse(files(".semantic-boundary.json")) \ "boundary_version") == JInt(1))
    assert(files(".semantic-clean.sv").contains("assign a_clock = b_forwarded;"))
    assert("\\.clock\\s*\\(a_clock\\)".r.findFirstIn(files(".semantic-clean.sv")).nonEmpty)
    val reordered = forwardedSource.replace("inst a of Leaf\n    inst b of Leaf", "inst b of Leaf\n    inst a of Leaf")
    assert(entries(SemanticStateInventory.discover(lower(reordered), "Harness.dut", true)) == entries(manifest()))
    // Clock has the same local name in each scope; it is not an alias cycle.
    val nested = forwardedSource.replace("  module InventoryDUT :", """  module Relay :
    input clock : Clock
    output forwarded : Clock
    inst leaf of Leaf
    leaf.clock <= clock
    leaf.reset <= UInt<1>(0)
    leaf.data <= UInt<9>(0)
    forwarded <= leaf.forwarded

  module InventoryDUT :""")
      .replace("    inst a of Leaf", "    inst relay of Relay\n    relay.clock <= clock\n    inst a of Leaf")
      .replace("    b.clock <= clock", "    b.clock <= relay.forwarded")
    val es = entries(SemanticStateInventory.discover(lower(nested), "Harness.dut", true))
    assert(es.size == 9 && es.forall(e => (e \ "clock_domain") == JString("Harness.dut.clock")))
    assert(es.exists(e => (e \ "hierarchy") == JString("Harness.dut.relay.leaf.saved")))
    // The ordinary opt-in retains its output-clock rejection contract.
    intercept[IllegalArgumentException](SemanticStateInventory.discover(lower(forwardedSource), "Harness.dut"))
  }

  it should "resolve a driven child input clock pin in the parent without accepting data or cyclic pins" in {
    val src = forwardedSource.replace("a.clock <= b.forwarded", "a.clock <= b.clock")
    assert(entries(SemanticStateInventory.discover(lower(src), "Harness.dut", true)) == entries(manifest()))
    for ((text, cycle) <- Seq(
      src.replace("b.clock <= clock", "b.clock <= a.clock") -> true,
      src.replace("b.clock <= clock", "b.clock <= asClock(bits(data, 0, 0))") -> false,
      src.replace("b.clock <= clock", "b.clock is invalid") -> false)) {
      val error = intercept[IllegalArgumentException](SemanticStateInventory.discover(
        lower(text, injectClockLoop = cycle), "Harness.dut", true))
      assert(error.getMessage.contains("clock"), error.getMessage)
    }
  }

  private def clockPinSource: String = forwardedSource
    .replace("  module Leaf :", """  module ClockPin :
    input pad : UInt<1>
    output value : UInt<1>
    value <= pad

  module Leaf :""")
    .replace("    forwarded <= clock", "    inst pin of ClockPin\n    pin.pad <= asUInt(clock)\n    forwarded <= asClock(pin.value)")

  it should "prove transparent clock bitcasts and concrete one-bit IO cells without treating data as a clock" in {
    for (src <- Seq(clockPinSource,
                    forwardedSource.replace("forwarded <= clock", "forwarded <= asClock(asUInt(clock))"),
                    clockPinSource.replace("asUInt(clock)", "bits(asUInt(clock), 0, 0)"))) {
      assert(entries(SemanticStateInventory.discover(lower(src), "Harness.dut", true)) == entries(manifest()))
    }
    val negatives = Seq(
      clockPinSource.replace("value <= pad", "value <= not(pad)"),
      clockPinSource.replace("value <= pad", "value <= and(pad, gate)")
        .replace("    input pad : UInt<1>", "    input pad : UInt<1>\n    input gate : UInt<1>")
        .replace("    pin.pad <= asUInt(clock)", "    pin.gate <= bits(data, 0, 0)\n    pin.pad <= asUInt(clock)"),
      clockPinSource.replace("    pin.pad <= asUInt(clock)", "    pin.pad <= bits(data, 0, 0)"),
      clockPinSource.replace("    pin.pad <= asUInt(clock)", "    pin.pad <= UInt<1>(0)"),
      clockPinSource.replace("    pin.pad <= asUInt(clock)",
        "    reg delayed : UInt<1>, clock\n    delayed <= asUInt(clock)\n    pin.pad <= delayed"),
    )
    negatives.foreach { src =>
      val error = intercept[IllegalArgumentException](SemanticStateInventory.discover(lower(src), "Harness.dut", true))
      assert(error.getMessage.contains("clock"), error.getMessage)
    }
  }

  it should "reject cyclic, gated, undriven and opaque hierarchical clock aliases without emitting artifacts" in {
    val cases = Seq(
      forwardedSource.replace("b.clock <= clock", "b.clock <= a.forwarded") -> "clock alias cycle",
      forwardedSource.replace("forwarded <= clock", "forwarded <= asClock(bits(data, 0, 0))") -> "clock",
      forwardedSource.replace("forwarded <= clock", "forwarded is invalid") -> "clock",
      forwardedSource.replace("    b.clock <= clock", "    b.clock <= b.forwarded") -> "clock alias cycle",
      forwardedSource.replace("  module Leaf :", "  extmodule Leaf :")
        .replace("    reg saved : UInt<9>, clock with :\n      reset => (reset, UInt<9>(0))\n    saved <= data\n    forwarded <= clock\n    value <= saved", "    defname = OpaqueClockLeaf") -> "opaque external module",
      forwardedSource.replace("    input wen : UInt<1>", "    input wen : UInt<1>\n    input otherClock : Clock")
        .replace("b.clock <= clock", "b.clock <= otherClock")
        .replace("dut.wen <= wen", "dut.wen <= wen\n    dut.otherClock <= otherClock") -> "multiple target clock domains",
      forwardedSource.replace("    input wen : UInt<1>", "    input wen : UInt<1>\n    input otherClock : Clock")
        .replace("    output count : UInt<9>", "    output bridgeClock : Clock\n    output count : UInt<9>")
        .replace("    count <= counter", "    bridgeClock <= otherClock\n    count <= counter")
        .replace("dut.wen <= wen", "dut.wen <= wen\n    dut.otherClock <= otherClock") -> "outside the state clock domain",
    )
    cases.foreach { case (src, message) =>
      val state = lower(src, injectClockLoop = message == "clock alias cycle")
        .copy(annotations = Seq(SemanticBridgeCheckpointRootAnnotation("Harness.dut")))
      val error = intercept[IllegalArgumentException](new BridgeSemanticStateInventory().execute(state))
      assert(error.getMessage.contains(message), error.getMessage)
      assert(!state.annotations.exists(_.isInstanceOf[GoldenGateOutputFileAnnotation]))
    }
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

  private def memorySource(port: String, policy: String): String = {
    val rw = port == "readwriter"
    s"""circuit Top :
  module Top :
    input clock : Clock
    input enable : UInt<1>
    input address : UInt<3>
    input data : UInt<9>
    input write : UInt<1>
    output result : UInt<9>
    mem ram :
      data-type => UInt<9>
      depth => 5
      read-latency => 1
      write-latency => 1
      $port => p
      ${if (rw) "" else "writer => w"}
      read-under-write => $policy
    ram.p.clk <= clock
    ram.p.en <= enable
    ram.p.addr <= address
    ${if (rw) "ram.p.wmode <= write\n    ram.p.wmask <= UInt<1>(1)\n    ram.p.wdata <= data"
      else "ram.w.clk <= clock\n    ram.w.en <= write\n    ram.w.addr <= address\n    ram.w.mask <= UInt<1>(1)\n    ram.w.data <= data"}
    result <= ram.p.${if (rw) "rdata" else "data"}
"""
  }

  it should "own passthrough read and readwrite storage in the emitted clean reference without altering the IR" in {
    for ((port, policy, field, width) <- Seq(("reader", "old", "data", 9),
        ("reader", "new", "addr", 3), ("reader", "undefined", "addr", 3),
        ("readwriter", "old", "rdata", 9), ("readwriter", "undefined", "rdata", 9))) {
      val state = lower(memorySource(port, policy)).copy(annotations = Seq(SemanticCheckpointRootAnnotation("Top")))
      val result = SemanticStateInventory(state)
      assert(result.circuit == state.circuit)
      val files = result.annotations.collect { case a: GoldenGateOutputFileAnnotation => a.fileSuffix -> a.body }.toMap
      val es = entries(files(".semantic-state.json"))
      assert(es.size == 2 && es.map(e => (e \ "hierarchy").values).toSet == Set("Top.ram", s"Top.ram_p_$field"))
      val saved = es.find(e => (e \ "hierarchy") == JString(s"Top.ram_p_$field")).get
      assert((saved \ "width") == JInt(width) && (saved \ "depth") == JInt(1))
      assert((saved \ "reset") == JString("none") && (saved \ "clock_domain") == JString("Top.clock"))
      assert(files(".semantic-clean.sv").contains(s"reg [${width - 1}:0] ram_p_$field;"))
      assert(files(".semantic-clean.sv").contains("reg [8:0] ram [0:4];"))
      assert(files(".semantic-reference.fir") == state.circuit.serialize)
      sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
        val output = Paths.get(dir)
        Files.createDirectories(output)
        files.foreach { case (suffix, body) =>
          Files.write(output.resolve(s"semantic-$port-$policy" + suffix), body.getBytes(UTF_8))
        }
      }
    }
  }

  it should "reject unsupported memory policies, latencies and port clocks before publishing state" in {
    val src = memorySource("readwriter", "old")
    val cases = Seq(
      memorySource("readwriter", "new") -> "memory latency",
      src.replace("read-latency => 1", "read-latency => 0") -> "memory latency",
      src.replace("read-latency => 1", "read-latency => 2") -> "memory latency",
      src.replace("write-latency => 1", "write-latency => 2") -> "memory latency",
      src.replace("ram.p.clk <= clock", "ram.p.clk <= asClock(enable)") -> "clock",
      src.replace("    mem ram :", "    reg ram_p_rdata : UInt<9>, clock\n    ram_p_rdata <= data\n    mem ram :") -> "duplicate StateID",
      memorySource("reader", "old").replace("    input enable : UInt<1>",
        "    input otherClock : Clock\n    input enable : UInt<1>")
        .replace("ram.w.clk <= clock", "ram.w.clk <= otherClock") -> "multiple memory clocks",
    )
    for ((text, message) <- cases) {
      val state = lower(text).copy(annotations = Seq(SemanticCheckpointRootAnnotation("Top")))
      val error = intercept[IllegalArgumentException](SemanticStateInventory(state))
      assert(error.getMessage.contains(message), error.getMessage)
      assert(!state.annotations.exists(_.isInstanceOf[GoldenGateOutputFileAnnotation]))
    }
  }

  it should "reject opaque state, async reset, derived clocks, multiple clocks, and unsupported memory pipelines" in {
    val opaque = source.replace("module Leaf :", "extmodule Leaf :")
      .replace("    reg saved : UInt<9>, clock with :\n      reset => (reset, UInt<9>(0))\n    saved <= data\n    value <= saved", "    defname = OpaqueLeaf")
    val negatives = Seq(
      opaque -> "opaque external module",
      source.replace("input reset : UInt<1>", "input reset : AsyncReset") -> "reset",
      source.replace("a.clock <= clock", "a.clock <= asClock(bits(data, 0, 0))") -> "clock",
      source.replace("    input wen : UInt<1>", "    input wen : UInt<1>\n    input otherClock : Clock")
        .replace("b.clock <= clock", "b.clock <= otherClock")
        .replace("dut.wen <= wen", "dut.wen <= wen\n    dut.otherClock <= otherClock") -> "multiple target clock domains",
      source.replace("read-latency => 0", "read-latency => 2") -> "memory latency",
    )
    negatives.foreach { case (src, message) =>
      val error = intercept[IllegalArgumentException](manifest(src))
      assert(error.getMessage.contains(message), error.getMessage)
    }
  }
}
