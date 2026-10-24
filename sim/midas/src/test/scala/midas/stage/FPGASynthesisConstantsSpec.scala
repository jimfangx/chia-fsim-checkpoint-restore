// See LICENSE for license details.
package midas.stage

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.{CircuitName, ModuleName, ReferenceTarget}
import firrtl.ir.ExtModule
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.BlackBoxInlineAnno
import midas.checkpoint._
import midas.stage.phases.ConfigParametersAnnotation
import midas.targetutils.xdc.XDCPathToCircuitAnnotation
import org.chipsalliance.cde.config.Parameters
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class FPGASynthesisConstantsSpec extends AnyFlatSpec {
  private val model = new String(Files.readAllBytes(Paths.get(sys.props("checkpoint.plusarg.reference"))), UTF_8)
  private val source = """circuit FPGAConstants :
  extmodule Plus :
    output out : UInt<9>
    defname = plusarg_reader
    parameter WIDTH = 9
    parameter DEFAULT = 341
    parameter FORMAT = "checkpoint_constant=%d"
  module FPGAConstants :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<17>
    output result : UInt<32>
    output fixed : UInt<9>
    inst a of Plus
    inst b of Plus
    reg count : UInt<32>, clock with :
      reset => (reset, UInt<32>(0))
    count <= add(count, add(data, a.out))
    result <= count
    fixed <= b.out
"""
  private def lower(implementation: String = model): CircuitState =
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm, Seq(
      BlackBoxInlineAnno(ModuleName("Plus", CircuitName("FPGAConstants")), "plusarg_reader.v", implementation),
      firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private val request = SimulatorFPGASynthesisConstantsAnnotation
  private val inventory = SimulatorCheckpointInventoryAnnotation("FPGAConstants")
  private def compile(enabled: Boolean, bank: Boolean = false): firrtl.AnnotationSeq = {
    implicit val p: Parameters = Parameters.empty
    val state = lower()
    val inline = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").map(Paths.get(_).resolve("fpga-constants/inline"))
      .getOrElse(Files.createTempDirectory("fpga-constants-inline-"))
    Files.createDirectories(inline)
    new GoldenGateCompilerPhase().compileSimulator(state.copy(annotations = state.annotations ++ Seq(
      ConfigParametersAnnotation(p), XDCPathToCircuitAnnotation(None, None), inventory,
      firrtl.transforms.BlackBoxTargetDirAnno(inline.toString),
      midas.passes.HostClockSource(ReferenceTarget("FPGAConstants", "FPGAConstants", Nil, "clock", Nil)),
      firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])) ++
      (if (enabled) Seq(request) else Nil) ++ (if (bank) Seq(SimulatorBankProbeAnnotation) else Nil)))
  }
  private def file(a: firrtl.AnnotationSeq, suffix: String): String = a.collectFirst {
    case GoldenGateOutputFileAnnotation(body, `suffix`) => body
  }.get
  private def sv(a: firrtl.AnnotationSeq): String = a.collectFirst {
    case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value
  }.get

  behavior.of("Production FPGA synthesis-constant selection")
  it should "select only the explicit FPGA branch and bind complete repeated-instance provenance" in {
    val ordinary = compile(false)
    val selected = compile(true)
    val names = Seq("original.fir", "prepared.fir", "sources.json", "preparation.json", "emission.json")
      .map(".fpga-constants-" + _)
    assert(!ordinary.exists { case a: GoldenGateOutputFileAnnotation => a.fileSuffix.startsWith(".fpga-constants-"); case _ => false })
    assert(file(ordinary, ".simulator-state.json").contains("myplus"))
    assert(!file(selected, ".simulator-state.json").contains("myplus"))
    assert(file(ordinary, ".simulator-reference.fir").contains("extmodule Plus"))
    assert(file(selected, ".fpga-constants-prepared.fir").contains("module Plus"))
    assert(!file(selected, ".simulator-reference.fir").contains("extmodule Plus"))
    val report = parse(file(selected, ".fpga-constants-preparation.json"))
    assert((report \ "complete_fpga_capture") == JBool(false))
    assert((report \ "original_reference_sha256") == JString(SemanticStateInventory.sha256(file(selected, names(0)))))
    assert((report \ "prepared_reference_sha256") == JString(SemanticStateInventory.sha256(file(selected, names(1)))))
    val rows = (report \ "constants").asInstanceOf[JArray].arr
    assert(rows.size == 1 && (rows.head \ "reachable_instances") ==
      JArray(List(JString("FPGAConstants.a"), JString("FPGAConstants.b"))))
    val binding = parse(file(selected, names.last))
    assert((binding \ "emitted_verilog_sha256") == JString(SemanticStateInventory.sha256(sv(selected))))
    names.dropRight(1).foreach(n => assert((binding \ "inputs" \ n) ==
      JString(SemanticStateInventory.sha256(file(selected, n)))))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("fpga-constants"); Files.createDirectories(out)
      names.foreach(n => Files.write(out.resolve(n.drop(1)), file(selected, n).getBytes(UTF_8)))
      for ((name, text) <- Seq("selected.sv" -> sv(selected), "ordinary.sv" -> sv(ordinary),
          "selected-reference.fir" -> file(selected, ".simulator-reference.fir"),
          "selected-manifest.json" -> file(selected, ".simulator-state.json"),
          "plusarg_reader.v" -> model)) Files.write(out.resolve(name), text.getBytes(UTF_8))
    }
  }

  it should "require one whole-root inventory and reject duplicate or semantic requests before lowering" in {
    val state = lower()
    for (annotations <- Seq(Seq(request), Seq(request, request, inventory),
        Seq(request, SimulatorCheckpointInventoryAnnotation("Plus")), Seq(request, inventory, inventory),
        Seq(request, inventory, SemanticFAMEStateMapAnnotation))) {
      intercept[IllegalArgumentException] { SimulatorFPGASynthesisConstants.validateRequest(state.copy(annotations = annotations)) }
    }
    SimulatorFPGASynthesisConstants.validateRequest(state)
    val duplicate = intercept[IllegalArgumentException] {
      new GoldenGateCompilerPhase().transform(Seq(firrtl.stage.FirrtlCircuitAnnotation(state.circuit),
        firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.VerilogEmitter),
        request, request, inventory))
    }
    assert(duplicate.getMessage.contains("exactly one synthesis-constant"))
    val prepared = SimulatorFPGASynthesisConstants(state.copy(annotations = state.annotations ++ Seq(request, inventory)))
    intercept[IllegalArgumentException] { SimulatorFPGASynthesisConstants(prepared) }
  }

  it should "retain FPGA provenance through production bank specialization and final inventory" in {
    val selected = compile(true, bank = true)
    assert(sv(selected).contains("ckpt_freeze") && sv(selected).contains("ckpt_clock"))
    val manifest = parse(file(selected, ".bank-probe-state.json"))
    assert((manifest \ "state").asInstanceOf[JArray].arr.size == 1)
    assert((parse(file(selected, ".fpga-constants-emission.json")) \ "emitted_verilog_sha256") ==
      JString(SemanticStateInventory.sha256(sv(selected))))
    assert(!file(selected, ".simulator-state.json").contains("myplus"))
  }

  it should "reject unknown source bytes and an empty selection without emitting a false receipt" in {
    for (state <- Seq(lower(model + "\n"), lower().copy(circuit = lower().circuit.copy(
        modules = lower().circuit.modules.filterNot(_.isInstanceOf[ExtModule]))))) {
      intercept[IllegalArgumentException] {
        SimulatorFPGASynthesisConstants(state.copy(annotations = state.annotations ++ Seq(request, inventory)))
      }
    }
  }

  it should "reject missing or duplicate source sidecars when binding final emitted RTL" in {
    val state = lower()
    val prepared = SimulatorFPGASynthesisConstants(state.copy(annotations = state.annotations ++ Seq(request, inventory)))
    val sidecar = prepared.annotations.collectFirst { case a: GoldenGateOutputFileAnnotation => a }.get
    for (annotations <- Seq(prepared.annotations.filterNot(_ == sidecar), prepared.annotations :+ sidecar)) {
      intercept[IllegalArgumentException] { SimulatorFPGASynthesisConstants.emissionBinding(
        prepared.copy(annotations = annotations), "module FPGAConstants; endmodule") }
    }
  }
}
