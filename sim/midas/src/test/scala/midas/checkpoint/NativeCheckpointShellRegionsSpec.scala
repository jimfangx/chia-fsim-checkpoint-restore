// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WRef}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointShellRegionsSpec extends AnyFlatSpec {
  import NativeCheckpointRegionsFixture.{source, targets}
  private val shells = Seq(ModuleTarget("NativeHost", "NativeHost"), ModuleTarget("NativeHost", "Stage"))
  private def generate = NativeCheckpointRegions.generate(source, targets, shellTargets = shells)
  behavior.of("Local shell banks composed with original native regions")

  it should "route both ancestor owners with child regions through one shared adapter without duplicating execution" in {
    val result = generate
    NativeCheckpointRegions.validate(result)
    assert(result.shells.map(_.entries.size) == Seq(1, 1))
    assert(result.descriptors.entries.size == 16 && result.descriptors.physicalClockGates == 4)
    assert(result.routing.regions.map(_.ceCount) == Seq(2, 2, 0, 0))
    val manifest = parse(result.descriptors.manifest)
    assert((manifest \ "uncovered_functional_state") == JArray(Nil))
    assert((manifest \ "uncovered_physical_gates") == JArray(Nil))
    val rows = (manifest \ "state").asInstanceOf[JArray].arr
    val local = rows.filter(r => (r \ "region") == JInt(2) || (r \ "region") == JInt(3))
    assert(local.map(_ \ "hierarchy").toSet == Set(JString("NativeHost.host_ticks"), JString("NativeHost.stage.control_ticks")))
    assert(local.forall(r => (r \ "width") == JInt(64) && (r \ "depth") == JInt(1)))
    assert(result.descriptors.entries.map(e => e.bank -> e.slot).distinct.size == 16)
    val before = GeneratedExternalOwners.instances(result.host, "NativeHost").map(_._1).toSet
    val after = GeneratedExternalOwners.instances(result.state, "NativeHost").map(_._1).toSet
    assert(before.subsetOf(after))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((manifest \ k) == JBool(false)))
    NativeCheckpointRegionsFixture.save(result, "native-shell-regions")
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-shell-regions")
    def save(name: String, body: String): Unit = { Files.write(out.resolve(name), body.getBytes(UTF_8)); () }
    result.shells.zipWithIndex.foreach { case (s, i) =>
      save(s"shell-$i-reference.fir", s.reference.circuit.serialize)
      save(s"shell-$i-generated.fir", s.state.circuit.serialize)
      save(s"shell-$i-binding.json", s.binding)
      save(s"shell-$i-mapping.json", s.mapping)
      save(s"shell-$i-banks.json", s.banks.binding)
    }
    save("source-annotations.json", JsonProtocol.serialize(result.host.annotations))
  }

  it should "reject duplicate local owners and local owners already included in a selected subtree" in {
    Seq(Seq(shells.head, shells.head), Seq(ModuleTarget("NativeHost", "NativeStorage")),
      Seq(ModuleTarget("Foreign", "NativeHost"))).foreach(ts =>
      intercept[IllegalArgumentException](NativeCheckpointRegions.generate(source, targets, shellTargets = ts)))
  }

  it should "reject native staging of mixed physical-clock regions before emitting a partial cut" in {
    val error = intercept[IllegalArgumentException](NativeCheckpointRegionGraft.generate(generate, stageNativeState = true))
    assert(error.getMessage.contains("shell-only endpoints"))
  }

  it should "reject nontransparent and unrelated local clocks before connecting shared raw readout" in {
    val h = source
    val changed = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case m: Module if m.name == "NativeHost" =>
        def replace(s: Statement): Statement = s match {
          case r: DefRegister if r.name == "host_ticks" => r.copy(clock = WRef("other_clock", ClockType))
          case s => s.mapStmt(replace)
        }
        m.copy(ports = m.ports :+ Port(NoInfo, "other_clock", Input, ClockType), body = replace(m.body))
      case m => m
    }))
    intercept[IllegalArgumentException](NativeCheckpointRegions.generate(changed, targets, shellTargets = shells))
  }

  it should "reconstruct local equations, ordering and annotation contracts after caller rehashing" in {
    val r = generate
    val binding = parse(r.binding)
    val changed = r.state.copy(circuit = r.state.circuit.copy(info = FileInfo(StringLit("bad-shell"))))
    val rehashed = SemanticStateInventory.canonical(binding merge JObject(List(
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(changed.circuit.serialize)))))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(state = changed, binding = rehashed)))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(shells = r.shells.reverse)))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(shells = r.shells.map(s =>
      s.copy(entries = s.entries.map(_.copy(slot = 1)))))))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(shells = Nil)))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(routing = r.routing.copy(
      regions = r.routing.regions.reverse))))
  }
}
