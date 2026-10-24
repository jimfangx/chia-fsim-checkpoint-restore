// See LICENSE for license details.
package midas.passes.xilinx

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.transforms.{BlackBoxInlineAnno, DontTouchAnnotation}
import midas.passes.DefineAbstractClockGate
import midas.checkpoint.SemanticStateInventory.sha256
import org.scalatest.flatspec.AnyFlatSpec

/** Reused by the production-bank fixture to retain independently auditable
  * historical circuits without carrying large circuit bodies in annotations.
  */
object ClockGateRetirementTestFixture {
  private val circuit = Parser.parse("""circuit GateTop :
  module GateTop :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    inst child of Child
    child.I <= I
    child.CE <= CE
    O <= child.O
  module Child :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    inst gate of AbstractClockGate_0
    gate.I <= I
    gate.CE <= CE
    O <= gate.O
  module Unreachable :
    inst deadGate of AbstractClockGate
""")
  private def working(s: Statement): Statement = s match {
    case i: DefInstance => WDefInstance(i.info, i.name, i.module, i.tpe)
    case other => other.mapStmt(working)
  }
  val source = BlackBoxInlineAnno(ModuleName("AbstractClockGate", CircuitName("GateTop")),
    "AbstractClockGate.v", DefineAbstractClockGate.impl)
  val input: CircuitState = CircuitState(circuit.copy(modules = circuit.modules.map {
    case m: Module => m.copy(body = working(m.body))
    case m => m
  } ++ Seq(DefineAbstractClockGate.blackbox,
    DefineAbstractClockGate.blackbox.copy(name = "AbstractClockGate_0"), DefineBUFGCE.blackbox)), LowForm,
    Seq(source))
  lazy val result: CircuitState = ReplaceAbstractClockGates.execute(input)
  lazy val receipt: XilinxClockGateRetirementAnnotation = result.annotations.collectFirst {
    case a: XilinxClockGateRetirementAnnotation => a
  }.get
  lazy val rewritten: Circuit = input.circuit.copy(modules = input.circuit.modules.map {
    case m: Module =>
      def rewrite(s: Statement): Statement = s match {
        case i: WDefInstance if Set("AbstractClockGate", "AbstractClockGate_0")(i.module) => i.copy(module = "BUFGCE")
        case other => other.mapStmt(rewrite)
      }
      m.copy(body = rewrite(m.body))
    case m => m
  })
}

class ClockGateRetirementSpec extends AnyFlatSpec {
  import ClockGateRetirementTestFixture._
  behavior.of("Actual Xilinx abstract clock-gate retirement")

  it should "rewrite every global declaration before retiring exact unused source and definitions" in {
    assert(result.circuit.modules.map(_.name) == Seq("GateTop", "Child", "Unreachable", "BUFGCE"))
    assert(!result.annotations.contains(source))
    assert(receipt.instances == Seq(RetiredClockGateInstance("Child", "gate", "AbstractClockGate_0", "BUFGCE"),
      RetiredClockGateInstance("Unreachable", "deadGate", "AbstractClockGate", "BUFGCE")))
    assert(receipt.inputSHA256 == sha256(input.circuit.serialize))
    assert(receipt.rewrittenSHA256 == sha256(rewritten.serialize))
    assert(receipt.outputSHA256 == sha256(result.circuit.serialize))
    assert(receipt.sourceText == DefineAbstractClockGate.impl)
    assert(result.circuit.modules.last == DefineBUFGCE.blackbox)
    assert(receipt.getClass.getInterfaces.contains(classOf[NoTargetAnnotation]))
  }

  it should "run through actual HostSpecialization and preserve unrelated inline sources" in {
    val extra = BlackBoxInlineAnno(ModuleName("Other", CircuitName("GateTop")), "Other.v", "module Other; endmodule")
    val before = input.copy(circuit = input.circuit.copy(modules = input.circuit.modules.filterNot(_.name == "BUFGCE")),
      annotations = input.annotations :+ extra)
    val actual = HostSpecialization.execute(before)
    assert(actual.circuit == result.circuit)
    assert(actual.annotations.contains(extra))
  }

  it should "reject modified missing duplicate foreign or renamed inline implementations" in {
    Seq(Seq.empty, Seq(source, source), Seq(source.copy(text = source.text + " ")),
      Seq(source.copy(name = "Other.v")), Seq(source.copy(target = ModuleName("AbstractClockGate", CircuitName("Foreign")))),
      Seq(source, source.copy(target = ModuleName("AbstractClockGate_0", CircuitName("GateTop")))))
      .foreach { annotations => intercept[IllegalArgumentException] {
        ReplaceAbstractClockGates.execute(input.copy(annotations = annotations))
      } }
  }

  it should "reject altered abstract definitions and physical-module collisions" in {
    val abstractGate = DefineAbstractClockGate.blackbox
    Seq(abstractGate.copy(ports = abstractGate.ports.reverse),
      abstractGate.copy(params = Seq(IntParam("UNKNOWN", 1)))) .foreach { changed =>
      intercept[IllegalArgumentException] { ReplaceAbstractClockGates.execute(input.copy(circuit =
        input.circuit.copy(modules = input.circuit.modules.map {
          case m if m.name == abstractGate.name => changed
          case m => m
        }))) }
    }
    intercept[IllegalArgumentException] { ReplaceAbstractClockGates.execute(input.copy(circuit =
      input.circuit.copy(modules = input.circuit.modules.filterNot(_.name == "BUFGCE")))) }
    intercept[IllegalArgumentException] { HostSpecialization.execute(input) }
  }

  it should "reject annotations targeting retiring modules or their instance paths" in {
    Seq(ModuleTarget("GateTop", "AbstractClockGate").ref("enable"),
      ModuleTarget("GateTop", "Child").instOf("gate", "AbstractClockGate_0").ref("O"))
      .foreach { target => intercept[IllegalArgumentException] {
        ReplaceAbstractClockGates.execute(input.copy(annotations = input.annotations :+ DontTouchAnnotation(target)))
      } }
    val raw = UnrecognizedAnnotation(org.json4s.native.JsonMethods.parse(
      """{"class":"unknown.TargetAnnotation","target":"~GateTop|AbstractClockGate>enable"}"""))
    intercept[IllegalArgumentException] { ReplaceAbstractClockGates.execute(input.copy(annotations = input.annotations :+ raw)) }
  }

  it should "reject repeated retirement and malformed historical receipts" in {
    intercept[IllegalArgumentException] { ReplaceAbstractClockGates.execute(result) }
    val corrupt = Seq(receipt.copy(inputSHA256 = "bad"), receipt.copy(definitions = receipt.definitions.reverse),
      receipt.copy(definitions = receipt.definitions :+ receipt.definitions.head), receipt.copy(sourceText = "module evil; endmodule"),
      receipt.copy(sourceTarget = "~Foreign|AbstractClockGate"), receipt.copy(sourceName = "Unknown.v"),
      receipt.copy(instances = receipt.instances.reverse), receipt.copy(instances = receipt.instances :+ receipt.instances.head),
      receipt.copy(instances = receipt.instances.map(_.copy(to = "FakeGate"))))
    corrupt.foreach(a => intercept[IllegalArgumentException] { ClockGateRetirement.context(a) })
  }

  it should "retain all source and mapping bytes through JSON serialization" in {
    val json = JsonProtocol.serialize(Seq(receipt))
    assert(JsonProtocol.deserialize(json) == Seq(receipt))
    assert(ClockGateRetirement.context(receipt) == ClockGateRetirement.context(
      JsonProtocol.deserialize(json).head.asInstanceOf[XilinxClockGateRetirementAnnotation]))
  }
}
