// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{CircuitState, LowForm, Parser}
import firrtl.annotations.ModuleTarget
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import midas.widgets.CheckpointCaptureDescriptors

class CheckpointRegionRoutingSpec extends AnyFlatSpec {
  private def entry(id: Int, bank: Int = 0, slot: Int = 0, width: Int = 64, depth: Int = 1) =
    StateAccessEntry(SemanticStateInventory.sha256(id.toString), bank, slot, width, depth)
  behavior.of("Native checkpoint region routing")
  it should "allocate independent functional banks and physical slots without changing endpoint addresses" in {
    val a = Seq(entry(0), entry(1, slot = 4, depth = 17), entry(2, 255, width = 1))
    val b = Seq(entry(3), entry(4, 1, 7, 19, 3), entry(5, 255, width = 1), entry(6, 255, 1, 1))
    val result = CheckpointRegionRouting.allocate(Seq(a, b))
    assert(result.regions == Seq(CheckpointRegionRouting.Region(0, 1, 0, 1),
      CheckpointRegionRouting.Region(1, 2, 1, 2)))
    assert(result.entries == Seq(a(0), a(1), b(0).copy(bank = 1), b(1).copy(bank = 2),
      a(2), b(2).copy(slot = 1), b(3).copy(slot = 2)))
    assert(result.parameters == ".REGIONS(2), .BANK_BASE(16'h0100), .BANK_COUNT(16'h0201), " +
      ".CE_BASE(32'h00010000), .CE_COUNT(32'h00020001)")
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("region-routing")
    Files.createDirectories(out)
    Files.write(out.resolve("routing.json"), result.binding.getBytes(UTF_8))
    Files.write(out.resolve("parameters.svh"), result.parameters.getBytes(UTF_8))
    val files = result.emit
    assert(files.size == 3 && files.head.fileSuffix == ".checkpoint-region-router.sv")
    Files.write(out.resolve("CheckpointRegionStateRouter.sv"), files.head.body.getBytes(UTF_8))
  }
  it should "reject overlapping identities, sparse banks, malformed CE owners and ABI overflow" in {
    val bad = Seq(Seq.empty[Seq[StateAccessEntry]], Seq(Seq.empty[StateAccessEntry]),
      Seq(Seq(entry(1)), Seq(entry(1))), Seq(Seq(entry(1), entry(2, slot = 0))),
      Seq(Seq(entry(1, 1))), Seq(Seq(entry(1), entry(2, 255, 1, 1))),
      Seq(Seq(entry(1), entry(2, 255))), Seq(Seq(entry(1), entry(2, 255, width = 1, depth = 2))))
    bad.foreach(b => intercept[IllegalArgumentException](CheckpointRegionRouting.allocate(b)))
    val many = (0 until 256).map(i => Seq(entry(i)))
    intercept[IllegalArgumentException](CheckpointRegionRouting.allocate(many))
    val overflow = Seq((0 until 255).map(i => entry(i, i)), Seq(entry(256)))
    intercept[IllegalArgumentException](CheckpointRegionRouting.allocate(overflow))
  }
  it should "compose exact Rocket DUT and FASED regions against their original global identities" in {
    val source = Paths.get(sys.props("checkpoint.inplace.rocket.reference"))
    val text = new String(Files.readAllBytes(source), UTF_8)
    val working = firrtl.passes.ToWorkingIR.run(Parser.parse(text))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val host = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    val top = ModuleTarget("F1Shim", "F1Shim").instOf("top", "FPGATop")
    val targets = Seq(top.instOf("sim", "SimWrapper"),
      top.instOf("FASEDMemoryTimingModel_0", "FASEDMemoryTimingModel"))
    val descriptors = targets.map { target =>
      val root = target.asPath.last._2.value
      val live = GeneratedExternalOwners.instances(host, root).map(_._2.name).toSet
      val reference = host.copy(circuit = host.circuit.copy(main = root,
        modules = host.circuit.modules.filter(m => live(m.name))), annotations = Nil)
      val manifest = SimulatorStateInventory.discover(reference)
      val plan = PhysicalClockCutPlan.discover(reference, manifest)
      val dependencies = PhysicalClockCutDependencies.discover(reference, manifest, plan)
      CheckpointCaptureDescriptors.fromNativeInPlace(host, target, reference, manifest, plan, dependencies)
    }
    val (combined, routing) = CheckpointCaptureDescriptors.fromNativeRegions(descriptors)
    val doc = parse(combined.manifest)
    val physical = (doc \ "uncovered_physical_gates").asInstanceOf[JArray].arr
    assert(physical.isEmpty && combined.physicalClockGates == 3)
    val before = (parse(descriptors.head.manifest) \ "uncovered_functional_state").asInstanceOf[JArray].arr
    val after = (doc \ "uncovered_functional_state").asInstanceOf[JArray].arr
    assert(after.nonEmpty && after.size < before.size)
    assert(combined.entries.size == descriptors.map(_.entries.size).sum)
    assert(combined.entries.map(_.stateID).distinct.size == combined.entries.size)
    assert(routing.regions.map(_.ceBase) == Seq(0, 2))
    val state = (doc \ "state").asInstanceOf[JArray].arr
    assert(state.map(_ \ "ordinal") == state.indices.map(i => JInt(i)))
    assert(state.filter(r => (r \ "domain") == JString("physical_ce")).map(_ \ "ce_bit") ==
      Seq(JInt(0), JInt(1), JInt(2)))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((doc \ k) == JBool(false)))
    intercept[IllegalArgumentException](CheckpointCaptureDescriptors.fromNativeRegions(Seq(descriptors.head, descriptors.head)))
    intercept[IllegalArgumentException](CheckpointCaptureDescriptors.fromNativeRegions(Seq(combined)))
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("region-rocket")
    Files.createDirectories(out)
    def save(name: String, value: String): Unit = Files.write(out.resolve(name), value.getBytes(UTF_8))
    save("source-input.json", s"""{"path":"${source.toAbsolutePath}","sha256":"${SemanticStateInventory.sha256(text)}","scope":"retained exact-recipe structural input; production ownership remains incomplete"}""")
    save("manifest.json", combined.manifest); save("binding.json", combined.binding)
    save("routing.json", routing.binding); save("parameters.svh", routing.parameters)
    save("host.fir", host.circuit.serialize)
    val globalManifest = SimulatorStateInventory.discover(host)
    val globalPlan = PhysicalClockCutPlan.discover(host, globalManifest)
    save("global-simulator-state.json", globalManifest)
    save("global-clock-plan.json", globalPlan)
    save("global-dependencies.json", PhysicalClockCutDependencies.discover(host, globalManifest, globalPlan))
    println(s"REGION_ROCKET entries=${combined.entries.size} remaining=${after.size} physical=${combined.physicalClockGates}")
  }
  it should "reject descriptors reconstructed against a different original host" in {
    val (host, reference) = NativeCheckpointInPlaceFixture.fixture
    val manifest = SimulatorStateInventory.discover(reference)
    val plan = PhysicalClockCutPlan.discover(reference, manifest)
    val dependencies = PhysicalClockCutDependencies.discover(reference, manifest, plan)
    def descriptors(h: CircuitState) = CheckpointCaptureDescriptors.fromNativeInPlace(h,
      NativeCheckpointInPlaceFixture.target, reference, manifest, plan, dependencies)
    val other = host.copy(circuit = host.circuit.copy(info = firrtl.ir.FileInfo(firrtl.ir.StringLit("foreign-host"))))
    val a = descriptors(host); val b = descriptors(other)
    assert(intercept[IllegalArgumentException](CheckpointCaptureDescriptors.fromNativeRegions(Seq(a, b)))
      .getMessage.contains("different original host"))
  }
}
