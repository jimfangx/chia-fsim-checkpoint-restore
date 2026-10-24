// See LICENSE for license details.
package midas.passes

import firrtl.{CircuitState, LowForm, Namespace, Parser, WDefInstance}
import firrtl.annotations._
import firrtl.ir._
import midas.checkpoint.SemanticStateInventory.sha256
import org.scalatest.flatspec.AnyFlatSpec

/** Also used by the production-backend fixture: its context comes from an
  * actual pass invocation, not a fabricated successful-link annotation.
  */
object PreLinkRenamingTestFixture {
  val input = Parser.parse("""circuit HistoricalWrapper :
  module Shared :
    skip
  extmodule External :
    defname = external_cell
  module HistoricalWrapper :
    inst left of Shared
    inst right of Shared
    inst ext of External
""")
  def namespace: Namespace = Namespace(Parser.parse("""circuit Inner :
  module Shared :
    skip
  module Shared_1 :
    skip
  module External :
    skip
  module Inner :
    skip
"""))
  def run(): CircuitState = PreLinkRenaming.execute(CircuitState(input, LowForm,
    Seq(PreLinkRenamingAnnotation(namespace))))
  def history: PreLinkRenamingResultAnnotation = run().annotations.collectFirst {
    case a: PreLinkRenamingResultAnnotation => a
  }.get
}

class PreLinkRenamingSpec extends AnyFlatSpec {
  import PreLinkRenamingTestFixture._
  behavior.of("Consumed pre-link namespace requests")

  it should "rename definitions and repeated instances and bind the exact historical circuits" in {
    val result = PreLinkRenamingTestFixture.run()
    val h = result.annotations.collectFirst { case a: PreLinkRenamingResultAnnotation => a }.get
    assert(!result.annotations.exists(_.isInstanceOf[PreLinkRenamingAnnotation]))
    assert(h.circuit == "HistoricalWrapper")
    assert(h.moduleRenames == Seq("Shared" -> "Shared_0", "External" -> "External_0",
      "HistoricalWrapper" -> "HistoricalWrapper"))
    assert(h.inputSHA256 == sha256(input.serialize) && h.outputSHA256 == sha256(result.circuit.serialize))
    assert(result.circuit.modules.map(_.name) == h.moduleRenames.map(_._2))
    val top = result.circuit.modules.last.asInstanceOf[Module]
    val instances = top.body.asInstanceOf[Block].stmts.collect { case i: WDefInstance => i.name -> i.module }
    assert(instances == Seq("left" -> "Shared_0", "right" -> "Shared_0", "ext" -> "External_0"))
    assert(result.circuit.modules.collectFirst { case m: ExtModule => m.defname }.contains("external_cell"))
    h.moduleRenames.foreach { case (from, to) =>
      val original = ModuleName(from, CircuitName(h.circuit))
      assert(result.renames.get.get(original).getOrElse(Seq(original)) ==
        Seq(ModuleName(to, CircuitName(h.circuit))))
    }
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(Seq(h))) == Seq(h))
  }

  it should "retain unrelated annotations and immutable history after the namespace changes" in {
    val ns = namespace
    val marker = firrtl.transforms.NoDedupAnnotation(ModuleTarget("HistoricalWrapper", "Shared"))
    val result = PreLinkRenaming.execute(CircuitState(input, LowForm, Seq(PreLinkRenamingAnnotation(ns), marker)))
    val before = JsonProtocol.serialize(result.annotations)
    ns.newName("Shared"); ns.newName("External")
    assert(before == JsonProtocol.serialize(result.annotations))
    assert(result.annotations.contains(marker))
  }

  it should "reject missing duplicate and colliding-top requests before mutating a namespace" in {
    val ns = namespace
    Seq(Nil, Seq.fill(2)(PreLinkRenamingAnnotation(ns))).foreach { requests =>
      assert(intercept[IllegalArgumentException] {
        PreLinkRenaming.execute(CircuitState(input, LowForm, requests))
      }.getMessage.contains("exactly one namespace request"))
    }
    assert(ns.newName("Shared") == "Shared_0")
    assert(intercept[IllegalArgumentException] {
      PreLinkRenaming.execute(CircuitState(input, LowForm, Seq(PreLinkRenamingAnnotation(Namespace(input)))))
    }.getMessage.contains("same name as parent's top"))
  }

  it should "keep an unconsumed or wrapped namespace request unsupported by bank probing" in {
    val state = CircuitState(Parser.parse("""circuit Top :
  module Top :
    input clock : Clock
    reg value : UInt<8>, clock
    value <= UInt<8>(1)
"""), LowForm)
    val request = PreLinkRenamingAnnotation(namespace)
    Seq[Annotation](request, DeletedAnnotation("midas.passes.PreLinkRenaming", request)).foreach { a =>
      assert(intercept[IllegalArgumentException] {
        midas.checkpoint.SimulatorBankProbe(state.copy(annotations =
          Seq(midas.checkpoint.SimulatorBankProbeAnnotation, a)))
      }.getMessage.contains(a.getClass.getName))
    }
  }
}
