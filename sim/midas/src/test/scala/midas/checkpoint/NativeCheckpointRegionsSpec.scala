// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.ModuleTarget
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

object NativeCheckpointRegionsFixture {
  def source: CircuitState = {
    val h = NativeCheckpointInPlaceFixture.accessFixture
    val root = h.circuit.modules.find(_.name == "NativeStorage").get.asInstanceOf[Module]
    val other = root.copy(name = "OtherStorage")
    val outputs = root.ports.filter(_.direction == Output).map(p => p.copy(name = "other_" + p.name))
    val modules = h.circuit.modules.map {
      case m: Module if m.name == "Stage" => m.copy(ports = m.ports ++ outputs,
        body = Block(Seq(m.body, WDefInstance(NoInfo, "peer", other.name, firrtl.Utils.module_type(other))) ++ root.ports.map { p =>
          val pin = WSubField(WRef("peer"), p.name, p.tpe)
          val input = p.name match {
            case "enable" => DoPrim(firrtl.PrimOps.Not, Seq(WRef(p.name, p.tpe)), Nil, p.tpe)
            case "data" => DoPrim(firrtl.PrimOps.Xor, Seq(WRef(p.name, p.tpe),
              UIntLiteral((BigInt(1) << 129) + BigInt("123456789abcdef0", 16), IntWidth(130))), Nil, p.tpe)
            case _ => WRef(p.name, p.tpe)
          }
          if (p.direction == Input) Connect(NoInfo, pin, input)
          else Connect(NoInfo, WRef("other_" + p.name, p.tpe), pin)
        }))
      case m: Module if m.name == "NativeHost" => m.copy(ports = m.ports ++ outputs,
        body = Block(Seq(m.body) ++ outputs.map(p => Connect(NoInfo, WRef(p.name, p.tpe),
          WSubField(WRef("stage"), p.name, p.tpe)))))
      case m => m
    } :+ other
    val working = firrtl.passes.ToWorkingIR.run(h.circuit.copy(modules = modules))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    h.copy(circuit = firrtl.passes.ResolveFlows.run(typed))
  }
  val targets = Seq(NativeCheckpointInPlaceFixture.target,
    ModuleTarget("NativeHost", "NativeHost").instOf("stage", "Stage").instOf("peer", "OtherStorage"))
  def save(result: NativeCheckpointRegions.Result, name: String): Unit = {
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve(name)
    Files.createDirectories(out)
    def save(n: String, s: String): Unit = Files.write(out.resolve(n), s.getBytes(UTF_8))
    save("host.fir", result.host.circuit.serialize)
    save("instrumented.fir", result.state.circuit.serialize)
    save("host-annotations.json", firrtl.annotations.JsonProtocol.serialize(result.host.annotations))
    save("instrumented-annotations.json", firrtl.annotations.JsonProtocol.serialize(result.state.annotations))
    save("binding.json", result.binding); save("manifest.json", result.descriptors.manifest)
    save("descriptor-binding.json", result.descriptors.binding); save("routing.json", result.routing.binding)
    result.regions.zipWithIndex.foreach { case (r, i) =>
      save(s"region-$i.fir", r.state.circuit.serialize)
      save(s"region-$i-binding.json", r.native.binding)
    }
    val graft = NativeCheckpointRegionGraft.generate(result)
    save("grafted.fir", graft.state.circuit.serialize)
    save("grafted-annotations.json", firrtl.annotations.JsonProtocol.serialize(graft.state.annotations))
    graft.files.foreach(f => save("FireSim-generated" + f.fileSuffix, f.body))
    val placement = midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, Some("shell/CL"))
    val constrained = midas.passes.WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+ placement))
    constrained.annotations.collect { case a: midas.passes.XDCOutputAnnotation => a }.foreach(a => save("instrumented" + a.suffix.get, a.fileBody))
    val emitter = new SemanticCleanEmitter
    val emitted = new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
    save("instrumented.sv", emitted)
    val graftedConstraints = midas.passes.WriteXDCFile.execute(graft.state.copy(
      annotations = graft.state.annotations :+ placement))
    val graftedEmitter = new SemanticCleanEmitter
    save("grafted.sv", new Compiler(Seq(firrtl.options.Dependency.fromTransform(graftedEmitter)),
      Forms.LowForm, Set(graftedEmitter)).execute(graftedConstraints.copy(annotations =
      graftedConstraints.annotations :+ firrtl.EmitCircuitAnnotation(graftedEmitter.getClass))).getEmittedCircuit.value)
    println(s"NATIVE_REGIONS out=$out regions=${result.regions.size} entries=${result.descriptors.entries.size}")
  }
}

class NativeCheckpointRegionsSpec extends AnyFlatSpec {
  import NativeCheckpointRegionsFixture._
  behavior.of("Native checkpoint disjoint in-place regions")
  it should "graft shared ancestors once with separate bank definitions, sidebands and original annotation owners" in {
    val h = source
    val r = NativeCheckpointRegions.generate(h, targets)
    val before = GeneratedExternalOwners.instances(h, h.circuit.main).toMap
    val after = GeneratedExternalOwners.instances(r.state, h.circuit.main).toMap
    assert(before.keys.forall(after.contains))
    assert(r.state.circuit.modules.map(_.name).distinct.size == r.state.circuit.modules.size)
    Seq("NativeHost", "Stage").foreach { name =>
      val original = h.circuit.modules.find(_.name == name).get.asInstanceOf[Module]
      val changed = r.state.circuit.modules.find(_.name == name).get.asInstanceOf[Module]
      assert(changed.body.serialize.startsWith(original.body.serialize))
      assert(changed.ports.take(original.ports.size) == original.ports)
      assert((0 until 2).forall(i => changed.ports.exists(_.name.startsWith(NativeCheckpointRegions.prefix(i)))))
    }
    assert(after("NativeHost.stage.network.left_").name.startsWith("CheckpointRegion0_"))
    assert(after("NativeHost.stage.peer.left_").name.startsWith("CheckpointRegion1_"))
    assert(after("NativeHost.stage.network.left_").name != after("NativeHost.stage.peer.left_").name)
    assert(r.descriptors.physicalClockGates == 4 && r.routing.regions.map(_.ceBase) == Seq(0, 2))
    val binding = parse(r.binding)
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
    save(r, "native-regions")
    NativeCheckpointRegions.validate(r)
    // A rehashed but structurally changed network must still fail reconstruction.
    val bad = r.state.copy(circuit = r.state.circuit.copy(info = FileInfo(StringLit("corrupt-network"))))
    val changed = SemanticStateInventory.canonical(binding merge JObject(List(
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(bad.circuit.serialize)))))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(state = bad, binding = changed)))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(state = r.state.copy(
      annotations = r.state.annotations.filterNot(_.isInstanceOf[midas.InternalXDCAnnotation])))))
    intercept[IllegalArgumentException](NativeCheckpointRegions.validate(r.copy(routing = r.routing.copy(regions = r.routing.regions.reverse))))
  }
  it should "reject duplicate, nested, empty and stale selections before composing banks" in {
    val h = source
    Seq(Seq.empty, Seq(targets.head, targets.head),
      Seq(targets.head, ModuleTarget("NativeHost", "NativeHost").instOf("stage", "Stage")),
      Seq(targets.head, ModuleTarget("NativeHost", "NativeHost").instOf("wrong", "Stage")))
      .foreach(ts => intercept[IllegalArgumentException](NativeCheckpointRegions.generate(h, ts)))
  }
  it should "reject unsupported sideband identifiers and occupied ancestor namespaces" in {
    val h = source
    Seq("", "capture_", "checkpoint_inplace_bad-name", "checkpoint_inplace_0;").foreach(n =>
      intercept[IllegalArgumentException](NativeCheckpointInPlaceContext.generate(h, targets.head, n)))
    val modules = h.circuit.modules.map {
      case m: Module if m.name == "Stage" => m.copy(ports = m.ports :+
        Port(NoInfo, NativeCheckpointRegions.prefix(1) + "occupied", Input, UIntType(IntWidth(1))))
      case m => m
    }
    intercept[IllegalArgumentException](NativeCheckpointRegions.generate(h.copy(circuit = h.circuit.copy(modules = modules)), targets))
  }
  it should "install the retained exact Rocket DUT and FASED banks into the same original host" in {
    val input = Paths.get(sys.props("checkpoint.inplace.rocket.reference"))
    val working = firrtl.passes.ToWorkingIR.run(firrtl.Parser.parse(new String(Files.readAllBytes(input), UTF_8)))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val host = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    val top = ModuleTarget("F1Shim", "F1Shim").instOf("top", "FPGATop")
    val ts = Seq(top.instOf("sim", "SimWrapper"), top.instOf("FASEDMemoryTimingModel_0", "FASEDMemoryTimingModel"))
    val result = NativeCheckpointRegions.generate(host, ts)
    assert(result.descriptors.entries.size == 10910 && result.descriptors.physicalClockGates == 3)
    assert((parse(result.descriptors.manifest) \ "uncovered_functional_state").asInstanceOf[JArray].arr.size == 1189)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-regions-rocket")
    Files.createDirectories(out)
    Seq("host.fir" -> host.circuit.serialize, "instrumented.fir" -> result.state.circuit.serialize,
      "binding.json" -> result.binding, "manifest.json" -> result.descriptors.manifest,
      "descriptor-binding.json" -> result.descriptors.binding).foreach { case (n, s) =>
      Files.write(out.resolve(n), s.getBytes(UTF_8))
    }
    println(s"NATIVE_REGIONS_ROCKET entries=${result.descriptors.entries.size} out=$out")
  }
}
