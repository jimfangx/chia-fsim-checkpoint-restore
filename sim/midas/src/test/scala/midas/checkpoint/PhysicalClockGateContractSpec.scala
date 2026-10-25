// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations._
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.{BlackBoxInlineAnno, BlackBoxPathAnno, BlackBoxResourceAnno}
import midas.InternalXDCAnnotation
import midas.passes.{WriteXDCFile, XDCOutputAnnotation}
import midas.targetutils.xdc.{XDCAnnotation, XDCFiles, XDCPathToCircuitAnnotation}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class PhysicalClockGateContractSpec extends AnyFlatSpec {
  private val source = """circuit PhysicalGateTop :
  extmodule Physical :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Parent :
    input clock : Clock
    input ce : UInt<1>
    input data : UInt<8>
    output out : UInt<8>
    inst gate of Physical
    gate.I <= clock
    gate.CE <= ce
    reg saved : UInt<8>, gate.O
    saved <= data
    out <= saved
  module PhysicalGateTop :
    input clock : Clock
    input ce_left : UInt<1>
    input ce_right : UInt<1>
    input data : UInt<8>
    output a : UInt<8>
    output b : UInt<8>
    inst left of Parent
    inst right of Parent
    left.clock <= clock
    left.ce <= ce_left
    left.data <= data
    right.clock <= clock
    right.ce <= ce_right
    right.data <= data
    a <= left.out
    b <= right.out
"""
  private def state: CircuitState = new Compiler(Forms.LowForm).execute(CircuitState(
    Parser.parse(source), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private def primitive(s: CircuitState): ExtModule = s.circuit.modules.collectFirst { case m: ExtModule => m }.get
  private def rt(module: String, ref: String): ReferenceTarget = ReferenceTarget("PhysicalGateTop", module, Nil, ref, Nil)
  private def preflight(s: CircuitState, refs: ReferenceTarget*): Unit =
    GeneratedStateBankAnnotations.preflight(s.copy(annotations = Seq(InternalXDCAnnotation(
      XDCFiles.Implementation, refs.map(_ => "{}").mkString(" "), refs: _*))))

  behavior.of("Native physical BUFGCE pin and state-owner contracts")
  it should "admit only exact reachable public pins and cells with correlated hierarchy" in {
    val s = state
    preflight(s, rt("Physical", "I"), rt("Physical", "CE"), rt("Physical", "O"))
    preflight(s, rt("Parent", "gate"), rt("Parent", "gate").field("O"))
    preflight(s, ModuleTarget("PhysicalGateTop", "Parent").instOf("gate", "Physical").ref("O"))
    preflight(s, ModuleTarget("PhysicalGateTop", "PhysicalGateTop")
      .instOf("left", "Parent").instOf("gate", "Physical").ref("O"))
    val rows = (parse(GeneratedExternalOwners.audit(s, s.circuit.main)) \ "reachable_external_instances").asInstanceOf[JArray].arr
    assert(rows.size == 2 && rows.forall(row => (row \ "physical_contract" \ "direct_state_readout") == JBool(false)))
    assert(rows.forall(row => (row \ "physical_contract" \ "metasim_state_equivalent") == JBool(false)))
    assert(rows.forall(row => (row \ "physical_capture_binding") == JString("unsupported")))
  }

  it should "translate shared primitive leaves without inventing clones or cross-instance pin products" in {
    val s = state
    val top = s.circuit.modules.collectFirst { case m: Module if m.name == s.circuit.main => m }.get
    val parent = s.circuit.modules.collectFirst { case m: Module if m.name == "Parent" => m }.get
    def rename(s: Statement): Statement = s match {
      case i: firrtl.WDefInstance => i.copy(module = if (i.name == "left") "LeftParent" else "RightParent")
      case other => other.mapStmt(rename)
    }
    val clones = Seq(
      GeneratedStateBankAnnotations.Clone(top.name, top.name, top.name, top.copy(body = rename(top.body))),
      GeneratedStateBankAnnotations.Clone(parent.name, top.name + ".left", "LeftParent", parent.copy(name = "LeftParent")),
      GeneratedStateBankAnnotations.Clone(parent.name, top.name + ".right", "RightParent", parent.copy(name = "RightParent")))
    val cell = InternalXDCAnnotation(XDCFiles.Synthesis, "set_property DONT_TOUCH true [get_cells {}]", rt("Parent", "gate"))
    val clocks = InternalXDCAnnotation(XDCFiles.Implementation,
      "create_generated_clock -name ck_{} -source [get_pins {}] -divide_by 1 [get_pins {}]",
      rt("Physical", "O"), rt("Physical", "I"), rt("Physical", "O"))
    val nested = XDCAnnotation(XDCFiles.Implementation, "set_max_delay 7.0 -from [get_pins {}]",
      ModuleTarget(top.name, "Parent").instOf("gate", "Physical").ref("O"))
    val original = s.copy(annotations = Seq(cell, clocks, nested))
    val (translated, mappings) = GeneratedStateBankAnnotations.translate(original, clones)
    val clockCopies = translated.collect { case a: InternalXDCAnnotation if a.formatString == clocks.formatString => a }
    assert(clockCopies.size == 2)
    assert(clockCopies.forall(_.argumentList.forall(_.module == top.name)))
    assert(clockCopies.map(_.argumentList.head.path.map(_._1.value)) ==
      Seq(Seq("left", "gate"), Seq("right", "gate")))
    assert(clockCopies.forall(a => a.argumentList.map(_.path).distinct.size == 1))
    val nestedCopies = translated.collect { case a: XDCAnnotation => a }
    assert(nestedCopies.size == 2 && nestedCopies.forall(_.argumentList.head.path.head._2.value == "Physical"))
    assert(nestedCopies.map(_.argumentList.head.module).toSet == Set("LeftParent", "RightParent"))
    val specialized = s.copy(circuit = s.circuit.copy(modules = clones.map(_.body) :+ primitive(s)), annotations = translated)
    GeneratedStateBankAnnotations.preflight(specialized)
    // Convert the public annotation just as the real stage does, then run the
    // actual downstream XDC consumer against the specialized instance graph.
    val internal = translated.map { case a: XDCAnnotation => InternalXDCAnnotation(a); case a => a }
    val emitted = WriteXDCFile.execute(specialized.copy(annotations = internal :+ XDCPathToCircuitAnnotation(None, None)))
    val files = emitted.annotations.collect { case a: XDCOutputAnnotation => a.suffix.get -> a.fileBody }.toMap
    for (side <- Seq("left", "right")) {
      assert(files(".synthesis.xdc").contains(s"set_property DONT_TOUCH true [get_cells $side/gate]"))
      assert(files(".implementation.xdc").contains(s"create_generated_clock -name ck_$side/gate/O -source [get_pins $side/gate/I] -divide_by 1 [get_pins $side/gate/O]"))
      assert(files(".implementation.xdc").contains(s"set_max_delay 7.0 -from [get_pins $side/gate/O]"))
    }
    assert(files(".implementation.xdc").split("create_generated_clock", -1).length == 3)
    val verilog = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(specialized.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value
    val reference = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("physical-gate"); Files.createDirectories(out)
      val artifacts = Seq("input.fir" -> s.circuit.serialize, "specialized.fir" -> specialized.circuit.serialize,
        "specialized.sv" -> verilog, "reference.sv" -> reference,
        "input-annotations.json" -> JsonProtocol.serialize(original.annotations),
        "annotations.json" -> JsonProtocol.serialize(translated), "mappings.json" -> SemanticStateInventory.canonical(mappings),
        "external-owners.json" -> GeneratedExternalOwners.audit(specialized, specialized.circuit.main)) ++ files.toSeq.map {
          case (suffix, body) => ("physical-gate" + suffix) -> body
        }
      artifacts.foreach { case (name, body) => Files.write(out.resolve(name), body.getBytes(UTF_8)) }
    }
  }

  it should "reject missing internal indexed foreign stale mismatched and cross-root physical pins" in {
    val s = state
    Seq(rt("Physical", "enable_latch"), rt("Physical", "O").field("hidden"), rt("Physical", "O").index(0),
      rt("Missing", "O"), rt("Physical", "O").copy(circuit = "Foreign"),
      ModuleTarget("PhysicalGateTop", "Parent").instOf("absent", "Physical").ref("O"),
      ModuleTarget("PhysicalGateTop", "Physical").instOf("gate", "Physical").ref("O"),
      ModuleTarget("PhysicalGateTop", "PhysicalGateTop").instOf("left", "Physical").ref("O"))
      .foreach(a => intercept[IllegalArgumentException] { preflight(s, a) })
    intercept[IllegalArgumentException] { preflight(s, rt("Physical", "I"), rt("Parent", "clock")) }
    val dead = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules :+ primitive(s).copy(name = "Dead")))
    intercept[IllegalArgumentException] { preflight(dead, rt("Dead", "O")) }
  }

  it should "reject parameter polarity port shape defname and source overrides instead of guessing by name" in {
    val s = state; val m = primitive(s)
    Seq(m.copy(params = Seq(StringParam("CE_TYPE", StringLit("ASYNC")))),
      m.copy(params = Seq(IntParam("IS_CE_INVERTED", 1))), m.copy(ports = m.ports.reverse),
      m.copy(ports = m.ports.updated(1, m.ports(1).copy(tpe = UIntType(IntWidth(2))))),
      m.copy(ports = m.ports.updated(0, m.ports(0).copy(direction = Output))),
      m.copy(defname = "Other"), m.copy(ports = m.ports :+ Port(NoInfo, "hidden", Output, UIntType(IntWidth(1)))))
      .foreach { changed => intercept[IllegalArgumentException] {
        PhysicalClockGateContract.validate(s, changed)
      } }
    val target = ModuleName(m.name, CircuitName(s.circuit.main))
    Seq[Annotation](BlackBoxInlineAnno(target, "BUFGCE.v", "module BUFGCE; endmodule"),
      BlackBoxPathAnno(target, "/unknown/BUFGCE.v"), BlackBoxResourceAnno(target, "/BUFGCE.v"))
      .foreach { a => intercept[IllegalArgumentException] { PhysicalClockGateContract.validate(s.copy(annotations = Seq(a)), m) } }
  }

  it should "reject physical bank generation even with valid constraints and a complete metasim inventory" in {
    val s = state.copy(annotations = Seq(InternalXDCAnnotation(XDCFiles.Implementation,
      "set_max_delay 7.0 -from [get_pins {}]", rt("Parent", "gate").field("O"))))
    val manifest = SimulatorStateInventory.discover(s)
    assert(manifest.contains("metasim-bufgce-v1"))
    val error = intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(s, manifest, separateAccessClock = true)
    }
    assert(error.getMessage.contains("PhysicalGateTop.left.gate requires a synthesized capture owner and common low-phase cut"))
    // Target-box and state-preservation annotations still require concrete
    // storage. A pin-constraint contract cannot admit an opaque state target.
    intercept[IllegalArgumentException] { GeneratedStateBankAnnotations.preflight(s.copy(annotations = Seq(
      midas.core.TargetBoxAnnotation(ModuleTarget(s.circuit.main, "Physical"))))) }
  }
}
