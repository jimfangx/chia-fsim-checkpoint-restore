// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import firrtl.{CircuitState, LowForm, Parser}
import firrtl.annotations.JsonProtocol
import firrtl.options.TargetDirAnnotation
import midas.passes.ResolveSimulationInline
import midas.stage.GoldenGateCompilerPhase
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointRegionCompilerProbeSpec extends AnyFlatSpec {
  private val base = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("region-compiler-probe")
  Files.createDirectories(base)
  private val fixtureDir = Files.createTempDirectory(base, "widget-fixture-")
  private lazy val fixture = new NativeCheckpointRegionWidgetGraftSpec {
    override protected def fixtureOutput: Path = fixtureDir
  }
  private val selection = "ReservationShell.top.sim.network,ReservationShell.top.peer"
  private val request = NativeCheckpointRegionCompilerProbeAnnotation(selection)
  private def before(dir: Path, value: NativeCheckpointRegionCompilerProbeAnnotation = request): CircuitState = {
    val source = fixture.compilerProbeBefore(Nil)
    source.copy(annotations = source.annotations.filterNot(_.isInstanceOf[TargetDirAnnotation]) ++ Seq(TargetDirAnnotation(dir.toString), value))
  }
  private def text(path: Path): String = new String(Files.readAllBytes(path), UTF_8)
  private def status(dir: Path): JValue = parse(text(dir.resolve(NativeCheckpointRegionCompilerProbe.directory).resolve("status.json")))
  private def audit(dir: Path): Unit = {
    val output = dir.resolve(NativeCheckpointRegionCompilerProbe.directory)
    val doc = status(dir)
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported", "physical_fpga_acceptance").foreach(k => assert((doc \ k) == JBool(false)))
    val JObject(files) = doc \ "artifacts"
    assert(files.nonEmpty)
    files.foreach { case (name, hash) => assert(hash == JString(SemanticStateInventory.sha256(text(output.resolve(name))))) }
  }

  behavior.of("Actual compiler regional hookup diagnostic")

  it should "leave an absent request untouched and reject malformed duplicate overlapping or conflicting requests" in {
    val empty = CircuitState(Parser.parse("circuit Empty :\n  module Empty :\n    input clock : Clock\n"), LowForm)
    assert(NativeCheckpointRegionCompilerProbe(empty, empty) eq empty)
    assert(GoldenGateCompilerPhase.prepareCheckpointBoundary(empty) == empty)
    val bad = Seq("", "Root", "Root.a,", "Root..a", "Root.a,Root.a", "Root.a,Root.a.b", "Root.a;bad", "Root.a, Root.b")
    bad.foreach(s => intercept[IllegalArgumentException] {
      NativeCheckpointRegionCompilerProbe.validateCardinality(Seq(NativeCheckpointRegionCompilerProbeAnnotation(s)))
    })
    Seq(Seq(request, request), Seq(request, SimulatorBankProbeAnnotation), Seq(request, NativeFunctionalBankProbeAnnotation),
      Seq(request, PhysicalClockCutWiringProbeAnnotation), Seq(request, SemanticFAMEStateMapAnnotation)).foreach(as =>
      intercept[IllegalArgumentException](NativeCheckpointRegionCompilerProbe.validateCardinality(as)))
  }

  it should "write actual registered-widget source routes and identities before the unchanged compiler image guard rejects" in {
    val dir = Files.createTempDirectory(base, "guarded-")
    val source = before(dir)
    val error = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(source))
    assert(error.getMessage.contains("external transport state owner required before image emission"))
    val output = dir.resolve(NativeCheckpointRegionCompilerProbe.directory)
    assert((status(dir) \ "status") == JString("identity_bound_incomplete_image_guard_required"))
    assert(text(output.resolve("pre-inline.fir")) == source.circuit.serialize)
    val normalized = NativeCheckpointRegionCompilerProbe.normalizeMetadata(source)
    assert(text(output.resolve("pre-inline.anno.json")) == JsonProtocol.serialize(normalized.annotations))
    assert(Files.exists(output.resolve("metadata-normalization.json")))
    val post = ResolveSimulationInline.forCheckpointRegions(normalized)
    assert(text(output.resolve("post-inline.fir")) == post.circuit.serialize)
    assert(text(output.resolve("post-inline.anno.json")) == JsonProtocol.serialize(post.annotations))
    val inline = post.annotations.collectFirst { case a: midas.passes.SimulationInlineResultAnnotation => parse(a.body) }.get
    assert((inline \ "version") == JInt(3))
    assert((inline \ "regional_normalization" \ "input_sha256") == (inline \ "output_sha256"))
    val binding = parse(text(output.resolve("identity-binding.json")))
    assert((binding \ "descriptor_identity_bound") == JBool(true))
    assert(text(output.resolve("identity-bound.anno.json")).contains("Incomplete_checkpoint_hardware_coverage"))
    assert(Files.exists(output.resolve("generated.native-region-graft.sv")))
    assert(Files.exists(output.resolve("region-1-dependencies.json")))
    audit(dir)
    val retained = text(output.resolve("status.json"))
    intercept[java.nio.file.FileAlreadyExistsException](GoldenGateCompilerPhase.prepareCheckpointBoundary(source))
    assert(text(output.resolve("status.json")) == retained)
  }

  it should "retain exact phase snapshots and the original stale-target failure without inventing a widget owner" in {
    val dir = Files.createTempDirectory(base, "stale-")
    val source = before(dir, NativeCheckpointRegionCompilerProbeAnnotation("ReservationShell.top.missing"))
    val error = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(source))
    assert(error.getMessage.contains("stale concrete execution ReservationShell.top.missing"))
    assert((status(dir) \ "status") == JString("rejected"))
    assert((status(dir) \ "error") == JString(error.getMessage))
    audit(dir)
  }

  it should "reject altered inline history and retain its real source pair despite coherent snapshot hashes" in {
    val dir = Files.createTempDirectory(base, "history-")
    val source = before(dir)
    val post = ResolveSimulationInline(source)
    val changed = post.copy(circuit = post.circuit.copy(info = firrtl.ir.FileInfo(firrtl.ir.StringLit("changed"))))
    val error = intercept[IllegalArgumentException](NativeCheckpointRegionCompilerProbe(source, changed))
    assert(error.getMessage.contains("stale inline output_sha256"))
    assert((status(dir) \ "error") == JString(error.getMessage))
    audit(dir)
  }

  it should "prepare exact FPGA constants before recording the regional inline pair" in {
    val dir = Files.createTempDirectory(base, "constants-")
    val original = before(dir)
    val plus = Parser.parse("""circuit Constant :
  extmodule CompilerProbePlus :
    output out : UInt<9>
    defname = plusarg_reader
    parameter WIDTH = 9
    parameter DEFAULT = 341
    parameter FORMAT = "checkpoint_constant=%d"
""").modules.head
    val modules = original.circuit.modules.map {
      case m: firrtl.ir.Module if m.name == original.circuit.main => m.copy(body = firrtl.ir.Block(Seq(m.body,
        firrtl.WDefInstance(firrtl.ir.NoInfo, "probe_constant", plus.name, firrtl.Utils.module_type(plus)))))
      case m => m
    } :+ plus
    val model = text(Paths.get(sys.props("checkpoint.plusarg.reference")))
    val selected = original.copy(circuit = original.circuit.copy(modules = modules), annotations = original.annotations ++ Seq(
      firrtl.transforms.BlackBoxInlineAnno(firrtl.annotations.ModuleName(plus.name,
        firrtl.annotations.CircuitName(original.circuit.main)), "plusarg_reader.v", model),
      SimulatorFPGASynthesisConstantsAnnotation, SimulatorCheckpointInventoryAnnotation(original.circuit.main)))
    val error = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(selected))
    assert(error.getMessage.contains("external transport state owner required before image emission"))
    val output = dir.resolve(NativeCheckpointRegionCompilerProbe.directory)
    val prepared = NativeCheckpointRegionCompilerProbe.normalizeMetadata(SimulatorFPGASynthesisConstants(selected))
    assert(text(output.resolve("pre-inline.fir")) == prepared.circuit.serialize)
    assert(text(output.resolve("pre-inline.anno.json")) == JsonProtocol.serialize(prepared.annotations))
    assert(!text(output.resolve("pre-inline.fir")).contains("extmodule CompilerProbePlus"))
    assert((status(dir) \ "status") == JString("identity_bound_incomplete_image_guard_required"))
    audit(dir)
  }

  it should "retain reconstructed coverage when reserved entry and CE counts do not match the selected regions" in {
    val dir = Files.createTempDirectory(base, "counts-")
    val source = before(dir, NativeCheckpointRegionCompilerProbeAnnotation("ReservationShell.top.sim.network"))
    val error = intercept[IllegalArgumentException](GoldenGateCompilerPhase.prepareCheckpointBoundary(source))
    assert(error.getMessage.contains("reserved entry/gate count differs from reconstructed descriptors"))
    val output = dir.resolve(NativeCheckpointRegionCompilerProbe.directory)
    assert(Files.exists(output.resolve("descriptors.json")))
    assert(!Files.exists(output.resolve("identity-bound.fir")))
    assert((status(dir) \ "error") == JString(error.getMessage))
    audit(dir)
  }

  it should "retain physical and opaque transport rejection after a successful identity probe and annotation removal" in {
    val dir = Files.createTempDirectory(base, "stripped-")
    val source = before(dir)
    val bound = NativeCheckpointRegionCompilerProbe(source, ResolveSimulationInline(source))
    Seq(bound, bound.copy(annotations = Nil)).foreach { state =>
      assert(intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(state))
        .getMessage.contains("external transport state owner"))
      assert(intercept[IllegalArgumentException](PhysicalClockGateContract.requireCaptureOwner(state))
        .getMessage.contains("synthesized capture owner and common low-phase cut"))
    }
    val descriptors = parse(text(dir.resolve(NativeCheckpointRegionCompilerProbe.directory).resolve("descriptors.json")))
    assert((descriptors \ "uncovered_functional_state").asInstanceOf[JArray].arr.nonEmpty)
    assert((descriptors \ "unsupported_external_owners").asInstanceOf[JArray].arr.nonEmpty)
    audit(dir)
  }

  it should "resolve invisible expression metadata once while retaining exact original FIRRTL and annotation order" in {
    val parsed = Parser.parse("""circuit Metadata :
  module Metadata :
    input clock : Clock
    input data : UInt<8>
    output result : UInt<8>
    reg value : UInt<8>, clock
    value <= data
    result <= value
""")
    val original = CircuitState(firrtl.passes.ToWorkingIR.run(parsed), LowForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    val normalized = NativeCheckpointRegionCompilerProbe.normalizeMetadata(original)
    assert(normalized.circuit.serialize == original.circuit.serialize)
    assert(normalized.circuit != original.circuit)
    assert(normalized.annotations.take(original.annotations.size).toVector == original.annotations.toVector)
    val receipt = parse(normalized.annotations.collectFirst {
      case a: NativeCheckpointRegionCompilerMetadataAnnotation => a.body
    }.get)
    val changes = (receipt \ "changed_module_metadata").asInstanceOf[JArray].arr
    assert(changes.size == 1 && (changes.head \ "module") == JString("Metadata"))
    val difference = (changes.head \ "first_structural_difference").asInstanceOf[JString].s
    assert(difference.contains(".tpe") || difference.contains(".kind") || difference.contains(".flow"))
    assert((receipt \ "input_firrtl_sha256") == (receipt \ "output_firrtl_sha256"))
    val again = NativeCheckpointRegionCompilerProbe.normalizeMetadata(normalized.copy(annotations = original.annotations))
    assert(again.circuit == normalized.circuit)
    assert((parse(again.annotations.last.asInstanceOf[NativeCheckpointRegionCompilerMetadataAnnotation].body) \
      "changed_module_metadata") == JArray(Nil))
    intercept[IllegalArgumentException](NativeCheckpointRegionCompilerProbe.normalizeMetadata(normalized))
  }

  it should "diagnose hidden type and flow differences without accepting them as unchanged regional IR" in {
    val tpe = firrtl.ir.UIntType(firrtl.ir.IntWidth(8))
    val original = firrtl.WRef("data", tpe, firrtl.PortKind, firrtl.SourceFlow)
    val changedType = original.copy(tpe = firrtl.ir.UIntType(firrtl.ir.IntWidth(9)))
    val changedFlow = original.copy(flow = firrtl.SinkFlow)
    assert(original.serialize == changedType.serialize && original.serialize == changedFlow.serialize)
    assert(NativeCheckpointRegions.firstStructuralDifference(original, changedType).contains(".tpe"))
    assert(NativeCheckpointRegions.firstStructuralDifference(original, changedFlow).contains(".flow"))
  }
}
