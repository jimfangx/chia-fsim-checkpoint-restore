// See LICENSE for license details.
package midas.widgets

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import chisel3.stage.ChiselStage
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import firesim.lib.nasti.NastiParameters
import midas.checkpoint._
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointPhysicalDescriptorsSpec extends AnyFlatSpec {
  private val resource = Source.fromResource("checkpoint/InventoryDUT.firrtl")
  private val source = (try resource.mkString finally resource.close()).replace(
    "reg harnessOnly : UInt<1>, clock", "reg harnessOnly : UInt<1>, clock with :\n      reset => (reset, UInt<1>(0))")
  private val functionalState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)
  private val functionalManifest = SemanticStateInventory.discover(functionalState, functionalState.circuit.main)
  private val functionalResult = GeneratedStateBankAccess.generate(functionalState, functionalManifest)
  private val functional = CheckpointCaptureDescriptors.fromGenerated(functionalResult, functionalManifest)
  // Three concrete native gates; distinct from the functional-bank fixture.
  // This tests descriptor composition, never same-circuit production ownership.
  private val physicalState = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse("""circuit Physical :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Physical :
    input clock : Clock
    input enables : UInt<3>
    output values : UInt<3>
    inst a of Native
    inst b of Native
    inst c of Native
    a.I <= clock
    b.I <= clock
    c.I <= clock
    a.CE <= bits(enables, 0, 0)
    b.CE <= bits(enables, 1, 1)
    c.CE <= bits(enables, 2, 2)
    reg x : UInt<1>, a.O
    reg y : UInt<1>, b.O
    reg z : UInt<1>, c.O
    x <= not(x)
    y <= not(y)
    z <= not(z)
    values <= cat(z, cat(y, x))
"""), ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private val manifest = SimulatorStateInventory.discover(physicalState)
  private val plan = PhysicalClockCutPlan.discover(physicalState, manifest)
  private val dependencies = PhysicalClockCutDependencies.discover(physicalState, manifest, plan)
  private def compose = CheckpointCaptureDescriptors.withPhysicalClockState(functional, physicalState,
    manifest, plan, dependencies)
  private val out = sys.props.get("checkpoint.widget.out").map(Paths.get(_).resolve("physical-descriptors"))
    .getOrElse(Files.createTempDirectory("physical-descriptors-"))
  Files.createDirectories(out)
  private def save(name: String, text: String): Unit = {
    val path = out.resolve(name); Files.createDirectories(path.getParent)
    Files.write(path, text.getBytes(UTF_8))
  }

  behavior.of("Generated functional and physical CE descriptors")

  it should "preserve both ordered inventories and distinct immutable build bindings without ownership admission" in {
    val d = compose
    assert(d.entries.take(9) == functional.entries && d.entries.size == 12 && d.physicalClockGates == 3)
    assert(d.entries.takeRight(3).zipWithIndex.forall { case (e, i) =>
      e.bank == 255 && e.slot == i && e.width == 1 && e.depth == 1
    })
    assert(d.manifestSha256 == SemanticStateInventory.sha256(d.manifest))
    assert(d.bindingSha256 == SemanticStateInventory.sha256(d.binding))
    assert(d.manifestSha256 != functional.manifestSha256)
    val binding = parse(d.binding)
    assert((binding \ "functional_manifest") == parse(functionalManifest))
    assert((binding \ "functional_binding") == parse(functionalResult.binding))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach { k =>
      assert((binding \ k) == JBool(false) && (parse(d.manifest) \ k) == JBool(false))
    }
  }

  it should "reject repeated composition, stale physical provenance and either transport mode mismatch" in {
    val d = compose
    intercept[IllegalArgumentException] {
      CheckpointCaptureDescriptors.withPhysicalClockState(d, physicalState, manifest, plan, dependencies)
    }
    Seq((manifest + " ", plan, dependencies), (manifest, plan + " ", dependencies),
      (manifest, plan, dependencies + " ")).foreach { case (m, p, deps) =>
      intercept[IllegalArgumentException] {
        CheckpointCaptureDescriptors.withPhysicalClockState(functional, physicalState, m, p, deps)
      }
    }
    val cfg = CheckpointCaptureWidgetParams(12, d.manifestSha256, Seq("fixture.functional", "fixture.host"),
      descriptors = Some(d), physicalClockGates = 3)
    Seq(0, 1, 2, 4).foreach(n => intercept[IllegalArgumentException] { cfg.copy(physicalClockGates = n) })
    intercept[IllegalArgumentException] { cfg.copy(entries = 11) }
    intercept[IllegalArgumentException] { cfg.copy(manifestSha256 = functional.manifestSha256) }
    // Functional-only tables still cannot enable physical capture.
    intercept[IllegalArgumentException] {
      CheckpointCaptureWidgetParams(9, functional.manifestSha256, cfg.ownerNames,
        descriptors = Some(functional), physicalClockGates = 3)
    }
  }

  it should "emit the composed ROM and actual physical MMIO widget with matching host descriptors" in {
    val d = compose
    save("composition.json", d.manifest); save("composition-binding.json", d.binding)
    save("functional-manifest.json", functionalManifest); save("functional-binding.json", functionalResult.binding)
    save("functional-reference.fir", functionalState.circuit.serialize)
    save("functional-instrumented.fir", functionalResult.state.circuit.serialize)
    save("physical/reference.fir", physicalState.circuit.serialize)
    save("physical/manifest.json", manifest)
    save("physical/physical-clock-plan.json", plan)
    save("physical/physical-clock-dependencies.json", dependencies)
    PhysicalClockStateBank.emit(physicalState, manifest, plan, dependencies).foreach { f =>
      save("physical/" + f.fileSuffix.stripPrefix("."), f.body)
    }
    (new ChiselStage).emitVerilog(new CheckpointCaptureDescriptorROM(d) {
      override def desiredName = "PhysicalDescriptorROMProbe"
    }, Array("--target-dir", out.resolve("rom").toString))
    implicit val p: Parameters = Parameters.empty.alterPartial { case CtrlNastiKey => NastiParameters(32, 32, 4) }
    val cfg = CheckpointCaptureWidgetParams(12, d.manifestSha256, Seq("fixture.functional", "fixture.host"),
      descriptors = Some(d), physicalClockGates = 3)
    val widget = LazyModule(new CheckpointCaptureWidget(cfg))
    val rtl = (new ChiselStage).emitVerilog(widget.module, Array("--target-dir", out.resolve("widget").toString))
    assert(rtl.contains("module CheckpointCaptureDescriptorROM"))
    assert(rtl.contains(".EXTERNAL_CLOCK_NETWORK(1)"))
    val header = new StringBuilder
    widget.module.genHeader(BigInt(0x1000), Map.empty, header)
    save("physical_constants.h", header.toString)
    save("physical_manifest.inc", d.cppManifest)
    assert(header.toString.contains(d.bindingSha256) && header.toString.contains(d.cppManifest))
    assert(d.entries.forall(e => header.toString.contains(e.stateID)))
    println(s"PHYSICAL_DESCRIPTORS_ELABORATED out=$out functional=9 physical=3")
  }
}
