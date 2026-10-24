// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.scalatest.flatspec.AnyFlatSpec

class ClockedEffectsStateBankSpec extends AnyFlatSpec {
  private def lower(text: String) = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = {
    val result = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(state.copy(annotations = state.annotations :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    result.getEmittedCircuit.value
  }
  private def reference(state: CircuitState): CircuitState = {
    def rename(s: Statement): Statement = s match {
      case i: WDefInstance => i.copy(module = "Original" + i.module)
      case s => s.mapStmt(rename)
    }
    val preserved = if (state.circuit.main != "DenseStorage") Nil else {
      // The dense readout fixture intentionally has storage whose only external
      // observation is the checkpoint bank. Keep the independent, uninstrumented
      // oracle's matching storage through Verilog emission as well.
      state.circuit.modules.collect { case m: Module =>
        def registers(s: Statement): Seq[String] = s match {
          case r: DefRegister => Seq(r.name)
          case b: Block => b.stmts.flatMap(registers)
          case _ => Nil
        }
        registers(m.body).map { name => firrtl.transforms.DontTouchAnnotation(
          firrtl.annotations.ReferenceTarget("Original" + state.circuit.main, "Original" + m.name, Nil, name, Nil)) }
      }.flatten
    }
    state.copy(circuit = state.circuit.copy(main = "Original" + state.circuit.main,
      modules = state.circuit.modules.map {
        case m: Module => m.copy(name = "Original" + m.name, body = rename(m.body))
        case _ => throw new IllegalArgumentException("opaque reference")
      }), annotations = preserved)
  }
  private def saveArtifacts(state: CircuitState, label: String): Unit = {
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve(label); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> emit(reference(state)),
        "generated-bank-manifest.json" -> manifest, "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
  private val source = """circuit EffectsStorage :
  module Helper :
    input x : UInt<9>
    output y : UInt<9>
    y <= add(x, UInt<1>(1))
  module EffectsStorage :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<9>
    input eventEnable : UInt<1>
    input fault : UInt<1>
    output result : UInt<9>
    reg saved : UInt<9>, clock with :
      reset => (reset, UInt<9>(0))
    inst helper of Helper
    helper.x <= data
    saved <= helper.y
    result <= saved
    printf(clock, eventEnable, "CHECKPOINT_EFFECT data=%d\n", data)
    stop(clock, and(fault, not(eventEnable)), 1)
    assert(clock, not(fault), eventEnable, "CHECKPOINT_ASSERT")
"""
  behavior.of("Clocked effect and stateless hierarchy checkpoint contracts")
  it should "emit bounded fan-in selection for dense local state and repeated child banks" in {
    val registers = (0 until 257).map { i =>
      val width = i % 64 + 1
      s"""    reg word_$i : UInt<$width>, clock with :
      reset => (reset, UInt<$width>(${BigInt(i) & ((BigInt(1) << width) - 1)}))
    word_$i <= add(word_$i, add(data, UInt<64>(${i + 1})))"""
    }.mkString("\n")
    val children = Seq("a", "b", "c").map { name => s"""    inst child_$name of DenseLeaf
    child_$name.clock <= clock
    child_$name.reset <= reset
    child_$name.data <= data""" }.mkString("\n")
    val dense = s"""circuit DenseStorage :
  module DenseLeaf :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<64>
    output result : UInt<64>
    reg saved : UInt<64>, clock with :
      reset => (reset, UInt<64>(9))
    saved <= add(saved, data)
    result <= saved
  module DenseStorage :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<64>
    output result : UInt<64>
$registers
$children
    result <= add(word_0, child_a.result)
"""
    val state = lower(dense)
    val result = GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    assert(result.entries.size == 260 && result.state.circuit.modules.size == 4)
    val root = result.state.circuit.modules.collectFirst { case m: Module if m.name == "DenseStorage" => m }.get
    // 260 input words require nine pairwise OR stages; no state-dependent
    // serial priority mux remains in the readout. The odd tail is retained.
    val body = root.body.serialize
    assert(body.contains("ckpt_data_tree_8_0") && !body.contains("ckpt_data_tree_9_"))
    assert(body.contains("ckpt_match_tree_8_0") && !body.contains("ckpt_match_tree_9_"))
    assert(!body.contains("ckpt_data <= mux("))
    saveArtifacts(state, "dense")
  }

  it should "retain clockless helpers and gate print stop and verification enables with the same cut" in {
    val state = lower(source)
    val result = GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    assert(result.entries.size == 1 && result.state.circuit.modules.size == 2)
    val rtl = emit(result.state)
    assert(rtl.contains("CHECKPOINT_EFFECT") && rtl.contains("CHECKPOINT_ASSERT"))
    saveArtifacts(state, "effects")
  }
  it should "reject derived clocks hidden in a child of a single-clock root" in {
    val bad = source.replace("    input x : UInt<9>", "    input clock : Clock\n    input x : UInt<9>")
      .replace("    y <= add", "    reg hidden : UInt<1>, clock\n    hidden <= UInt<1>(1)\n    y <= add")
      .replace("    helper.x <= data", "    helper.clock <= asClock(bits(data, 0, 0))\n    helper.x <= data")
    assert(intercept[IllegalArgumentException] {
      val state = lower(bad)
      GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    }.getMessage.contains("computed clock data drives functional storage/effect at EffectsStorage.helper.hidden"))
  }
  it should "instrument the exact retained Rocket DTLB subtree including clockless barriers and assertions" in {
    val filename = sys.props.getOrElse("checkpoint.rocket.reference",
      fail("checkpoint.rocket.reference must name the retained exact-recipe FIRRTL input"))
      val input = scala.io.Source.fromFile(filename)
      val circuit = try Parser.parse(input.mkString) finally input.close()
      val modules = circuit.modules.map(m => m.name -> m).toMap
      val reachable = scala.collection.mutable.LinkedHashSet.empty[String]
      def walk(name: String): Unit = if (reachable.add(name)) {
        def visit(s: Statement): Unit = {
          s match { case i: DefInstance => walk(i.module); case _ => () }
          s.foreachStmt(visit)
        }
        visit(modules(name).asInstanceOf[Module].body)
      }
      walk("DTLB")
      val selected = circuit.copy(main = "DTLB", modules = circuit.modules.filter(m => reachable(m.name)))
      val state = lower(selected.serialize)
      val manifest = SimulatorStateInventory.discover(state)
      val result = GeneratedStateBankAccess.generate(state, manifest)
      assert(result.entries.size == 101)
      assert(result.state.annotations.contains(firrtl.transforms.NoCircuitDedupAnnotation))
      assert(!result.state.annotations.exists(_.isInstanceOf[firrtl.transforms.DedupedResult]))
      assert(result.state.circuit.modules.size == 17)
      assert(reachable.size > 3)
      saveArtifacts(state, "rocket-dtlb")
  }
}
