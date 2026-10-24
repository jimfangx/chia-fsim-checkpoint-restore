// See LICENSE for license details.
package midas.passes

import firrtl.{CircuitState, ChirrtlForm, Parser}
import firrtl.annotations._
import firrtl.stage.{Forms}
import firrtl.stage.transforms.Compiler
import firesim.lib.bridgeutils._
import midas.checkpoint.SemanticStateInventory.sha256
import org.scalatest.flatspec.AnyFlatSpec

/** Exercise the actual promote/commute/prune pass, including two instances
  * of the same bridge-bearing module. Export its historical inputs for an
  * independent Python audit of the production bank fixture.
  */
object BridgeExtractionTestFixture {
  private val circuit = Parser.parse("""circuit ExtractionFixture :
  extmodule ClockSource :
    output clock : Clock
    defname = ClockSource
  extmodule DataBridge :
    input clock : Clock
    input value : UInt<8>
    defname = DataBridge
  module Worker :
    input clock : Clock
    input value : UInt<8>
    inst bridge of DataBridge
    bridge.clock <= clock
    bridge.value <= value
  module ExtractionFixture :
    input value : UInt<8>
    inst clocks of ClockSource
    inst left of Worker
    inst right of Worker
    left.clock <= clocks.clock
    right.clock <= clocks.clock
    left.value <= value
    right.value <= value
""")
  val input: CircuitState = {
    val clock = ModuleTarget("ExtractionFixture", "ClockSource").ref("clock")
    val data = ModuleTarget("ExtractionFixture", "DataBridge")
    val annotations = Seq(
      BridgeAnnotation(clock.moduleTarget, Seq(ClockBridgeChannel("clock", Seq(clock),
        Seq(RationalClock("base", 1, 1)), Seq(1))), classOf[midas.widgets.ClockBridgeModule].getName, None),
      BridgeAnnotation(data, Seq(PipeBridgeChannel("value", data.ref("clock"),
        Seq(data.ref("value")), Nil, 0)), "fixture.DataBridgeModule", None))
    new Compiler(Forms.MidForm).execute(CircuitState(circuit, ChirrtlForm, annotations))
  }
  class Recording extends BridgeExtraction {
    var promoted: CircuitState = input
    var commuted: CircuitState = input
    override def commuteBridgeAnnotations(state: CircuitState): CircuitState = {
      promoted = state
      commuted = super.commuteBridgeAnnotations(state)
      commuted
    }
  }
  def run(): Recording = { val pass = new Recording; pass.execute(input); pass }
  def result(): CircuitState = new BridgeExtraction().execute(input)
  def history: BridgeExtractionResultAnnotation = result().annotations.collectFirst {
    case a: BridgeExtractionResultAnnotation => a
  }.get
  def connectivity(state: CircuitState): Seq[Annotation] = state.annotations.collect {
    case a: BridgeIOAnnotation => a
    case a: midas.passes.fame.FAMEChannelConnectionAnnotation => a
  }
}

class BridgeExtractionSpec extends AnyFlatSpec {
  import BridgeExtractionTestFixture._
  behavior.of("Consumed bridge extraction markers")

  it should "consume temporary instance markers and bind the actual extraction and connectivity" in {
    val pass = BridgeExtractionTestFixture.run()
    val result = pass.execute(input)
    val h = result.annotations.collectFirst { case a: BridgeExtractionResultAnnotation => a }.get
    assert(!result.annotations.exists(_.isInstanceOf[pass.BridgeInstance]))
    assert(h.circuit == "ExtractionFixture" && h.wrapper == "DummyTop")
    assert(h.inputSHA256 == sha256(input.circuit.serialize))
    assert(h.promotedSHA256 == sha256(pass.promoted.circuit.serialize))
    assert(h.outputSHA256 == sha256(result.circuit.serialize))
    assert(h.connectivitySHA256 == sha256(JsonProtocol.serialize(connectivity(pass.commuted))))
    assert(h.bridgeInstances.size == 3 && h.bridgeInstances.distinct.size == 3)
    assert(h.bridgeInstances.toSet == pass.promoted.annotations.collect {
      case pass.BridgeInstance(t) => t.serialize
    }.toSet)
    assert(!result.circuit.modules.exists(_.name == h.wrapper))
    assert(connectivity(result) == connectivity(pass.commuted))
    assert(connectivity(result).count(_.isInstanceOf[BridgeIOAnnotation]) == 3)
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(Seq(h))) == Seq(h))
  }

  it should "retain unrelated input annotations and stable history across repeated execution" in {
    val marker = firrtl.transforms.NoDedupAnnotation(ModuleTarget("ExtractionFixture", "Worker"))
    val pass = new BridgeExtraction
    val a = pass.execute(input.copy(annotations = input.annotations :+ marker))
    val b = pass.execute(input.copy(annotations = input.annotations :+ marker))
    assert(a.circuit == b.circuit && a.annotations.toSeq == b.annotations.toSeq)
    assert(a.annotations.contains(marker))
    assert(a.annotations.count(_.isInstanceOf[BridgeExtractionResultAnnotation]) == 1)
  }

  it should "retain an input module that collides with a wrapper from an earlier invocation" in {
    val pass = new BridgeExtraction
    pass.execute(input)
    val colliding = firrtl.ir.Module(firrtl.ir.NoInfo, "DummyTop", Nil, firrtl.ir.EmptyStmt)
    val result = pass.execute(input.copy(circuit = input.circuit.copy(modules = input.circuit.modules :+ colliding)))
    assert(result.circuit.modules.contains(colliding))
    assert(result.annotations.collectFirst { case a: BridgeExtractionResultAnnotation => a.wrapper }.contains("DummyTop_0"))
  }

  it should "reject a live or deleted extraction marker at the bank boundary" in {
    val pass = new BridgeExtraction
    val marker = pass.BridgeInstance(InstanceTarget("ExtractionFixture", "ExtractionFixture", Nil, "left", "Worker"))
    Seq[Annotation](marker, DeletedAnnotation("midas.passes.BridgeExtraction", marker)).foreach { a =>
      val state = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse("""circuit Top :
  module Top :
    input clock : Clock
    reg value : UInt<8>, clock
    value <= add(value, UInt<1>(1))
"""), ChirrtlForm)).copy(form = firrtl.LowForm)
      val error = intercept[IllegalArgumentException] {
        midas.checkpoint.SimulatorBankProbe(state.copy(annotations = Seq(
          midas.checkpoint.SimulatorBankProbeAnnotation, a)))
      }
      assert(error.getMessage.contains(a.getClass.getName))
    }
  }
}
