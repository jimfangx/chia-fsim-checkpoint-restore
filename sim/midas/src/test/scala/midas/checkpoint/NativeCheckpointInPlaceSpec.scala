// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, WRef}
import firrtl.annotations.{InstanceTarget, JsonProtocol, ModuleTarget, NoTargetAnnotation}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.scalatest.flatspec.AnyFlatSpec
import midas.InternalXDCAnnotation
import midas.targetutils.xdc.{RAMStyleHint, RAMStyles, XDCAnnotation, XDCFiles, XDCPathToCircuitAnnotation}
import midas.passes.{WriteXDCFile, XDCOutputAnnotation}

object NativeCheckpointInPlaceFixture {
  def reference: CircuitState = {
    val resource = scala.io.Source.fromResource("checkpoint/NativeStorage.firrtl")
    val source = try resource.mkString finally resource.close()
    new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source), ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  }
  def host(ref: CircuitState): CircuitState = {
    val root = ref.circuit.modules.find(_.name == ref.circuit.main).get
    def wrapper(name: String, child: String, instance: String, counter: String): String = {
      val ports = root.ports.map(p => "    " + p.serialize).mkString("\n")
      val connects = root.ports.map(p => if (p.direction == Input) s"    $instance.${p.name} <= ${p.name}"
        else s"    ${p.name} <= $instance.${p.name}").mkString("\n")
      s"""  module $name :
$ports
    output ${counter}_out : UInt<64>
    reg $counter : UInt<64>, clock with :
      reset => (reset, UInt<64>(0))
    $counter <= bits(add($counter, UInt<1>(1)), 63, 0)
    ${counter}_out <= $counter
    inst $instance of $child
$connects
"""
    }
    val outer = wrapper("NativeHost", "Stage", "stage", "host_ticks")
      .replace("    reg host_ticks", "    output control_ticks_out : UInt<64>\n    reg host_ticks")
      .replace("    inst stage of Stage", "    inst stage of Stage\n    control_ticks_out <= stage.control_ticks_out")
    val added = Parser.parse("circuit NativeHost :\n" + wrapper("Stage", root.name, "network", "control_ticks") + outer).modules
    val circuit = ref.circuit.copy(main = "NativeHost", modules = ref.circuit.modules ++ added)
    new Compiler(Forms.LowForm).execute(CircuitState(circuit, ChirrtlForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  }
  val target = ModuleTarget("NativeHost", "NativeHost").instOf("stage", "Stage")
    .instOf("network", "NativeStorage")
  def fixture: (CircuitState, CircuitState) = {
    val h = host(reference)
    val selected = h.copy(circuit = h.circuit.copy(main = "NativeStorage",
      modules = h.circuit.modules.filterNot(m => Set("Stage", "NativeHost")(m.name))))
    val top = ModuleTarget("NativeHost", "NativeHost")
    val leaf = ModuleTarget("NativeHost", "Leaf")
    val annotations = Seq(
      // A parent-rooted constraint correlates a raw control register with a
      // selected descendant; a module-local constraint expands once per leaf.
      InternalXDCAnnotation(XDCFiles.Implementation,
        "set_max_delay 4 -from [get_cells {}] -to [get_cells {}]",
        top.ref("host_ticks"), top.instOf("stage", "Stage").instOf("network", "NativeStorage").ref("cycles")),
      XDCAnnotation(XDCFiles.Implementation,
        "set_false_path -to [get_pins {}]", leaf.instOf("gate", "Native").ref("CE")),
      InternalXDCAnnotation(XDCFiles.Implementation,
        "set_max_delay 9 -from [get_pins {}] -to [get_pins {}]",
        ModuleTarget("NativeHost", "Native").ref("I"), ModuleTarget("NativeHost", "Native").ref("O")),
      XDCAnnotation(XDCFiles.Synthesis, RAMStyleHint.propertyTemplate(RAMStyles.DISTRIBUTED),
        top.instOf("stage", "Stage").instOf("network", "NativeStorage").ref("ram")))
    (h.copy(annotations = h.annotations ++ annotations), selected)
  }
  def accessFixture: CircuitState = {
    val (original, _) = fixture
    original.copy(annotations = original.annotations.map {
      case a: InternalXDCAnnotation if a.formatString ==
          "set_max_delay 4 -from [get_cells {}] -to [get_cells {}]" =>
        InternalXDCAnnotation(a.destinationFile,
          "set_max_delay 4 -from [get_cells {}_reg*] -to [get_cells {}_reg*]", a.argumentList: _*)
      case a: InternalXDCAnnotation if a.formatString ==
          "set_max_delay 9 -from [get_pins {}] -to [get_pins {}]" =>
        InternalXDCAnnotation(a.destinationFile,
          "set_property DONT_TOUCH true [get_cells -of_objects [get_pins {}]]; " +
          "set_property DONT_TOUCH true [get_cells -of_objects [get_pins {}]]", a.argumentList: _*)
      case a => a
    })
  }
}

class NativeCheckpointInPlaceSpec extends AnyFlatSpec {
  import NativeCheckpointInPlaceFixture._
  private def generate(h: CircuitState, r: CircuitState, t: InstanceTarget = target) = {
    val manifest = SimulatorStateInventory.discover(r)
    val plan = PhysicalClockCutPlan.discover(r, manifest)
    val deps = PhysicalClockCutDependencies.discover(r, manifest, plan)
    NativeCheckpointInPlace.generate(h, t, r, manifest, plan, deps)
  }
  private def emit(s: CircuitState): String = {
    val emitter = new SemanticCleanEmitter
    val constrained = WriteXDCFile.execute(s.copy(annotations = s.annotations :+ XDCPathToCircuitAnnotation(None, None)))
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Native checkpoint in-place instrumentation")
  it should "bind complete compiler context separately and reject downstream drift at either boundary" in {
    val (h, _) = fixture
    val contexts = Seq(
      midas.stage.ConfigPackageAnnotation("fixture"), midas.stage.ConfigStringAnnotation("NativeFixture"),
      midas.stage.GoldenGateOutputFileAnnotation("`define NATIVE_WIDTH 8\n", ".defines.vh"),
      midas.passes.ChannelClockInfoAnnotation(Map("native_channel" ->
        firesim.lib.bridgeutils.RationalClock("native_core", 2, 4))))
    val source = h.copy(annotations = h.annotations ++ contexts)
    val result = NativeCheckpointInPlaceContext.generate(source, target)
    NativeCheckpointInPlaceContext.validate(result, result.state)
    assert(result.state.circuit == result.native.state.circuit)
    assert(contexts.forall(result.state.annotations.contains))
    val changes = Seq(result.state.annotations.filterNot(_ == contexts.head),
      result.state.annotations.map {
        case a: midas.stage.ConfigStringAnnotation => a.copy(configString = "ChangedFixture")
        case a => a
      }, result.state.annotations :+ midas.stage.ConfigPackageAnnotation("changed"))
    changes.foreach { annotations =>
      val changed = result.state.copy(annotations = annotations)
      intercept[IllegalArgumentException](NativeCheckpointInPlaceContext.validate(result, changed))
      intercept[IllegalArgumentException](NativeCheckpointInPlaceContext.validate(result.copy(state = changed), result.state))
    }
    intercept[IllegalArgumentException](NativeCheckpointInPlaceContext.generate(
      source.copy(annotations = source.annotations :+ contexts.head), target))
    case object HiddenContextOwner extends NoTargetAnnotation
    assert(intercept[IllegalArgumentException](NativeCheckpointInPlaceContext.generate(
      source.copy(annotations = source.annotations :+ HiddenContextOwner), target)).getMessage
      .contains("explicit clone/rename translation"))
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-inplace-context")
    saveContext(result, out)
    println(s"NATIVE_INPLACE_CONTEXT out=$out retained_context=${contexts.size}")
  }

  it should "project the actual exact Rocket annotations and compiler context without granting image admission" in {
    val input = Paths.get(sys.props.getOrElse("checkpoint.inplace.rocket.production",
      fail("checkpoint.inplace.rocket.production must identify the current annotated exact-recipe source")))
    def read(suffix: String): String = new String(Files.readAllBytes(Paths.get(input.toString + suffix)), UTF_8)
    val working = firrtl.passes.ToWorkingIR.run(Parser.parse(read("-reference.fir")))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    // JsonProtocol omits the nested class hint in DeletedAnnotation. Recover
    // only the two stage-consumed input events, then require an exact JSON
    // round trip. A deleted owner or pass request is never a context exemption.
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val rawContext = parse(read("-context.anno.json")).asInstanceOf[JArray].arr
    val contexts = rawContext.flatMap { value =>
      if ((value \ "class") == JString("firrtl.annotations.DeletedAnnotation")) {
        val JString(file) = value \ "anno" \ "file"
        val annotation = (value \ "xFormName") match {
          case JString("firrtl.stage.phases.AddCircuit") => firrtl.annotations.DeletedAnnotation(
            "firrtl.stage.phases.AddCircuit", firrtl.stage.FirrtlFileAnnotation(file))
          case JString("firrtl.options.phases.GetIncludes") => firrtl.annotations.DeletedAnnotation(
            "firrtl.options.phases.GetIncludes", firrtl.options.InputAnnotationFileAnnotation(file))
          case _ => fail("unsupported deleted input context")
        }
        assert(parse(JsonProtocol.serialize(Seq(annotation))) == JArray(List(value)))
        Seq(annotation)
      } else JsonProtocol.deserialize(SemanticStateInventory.canonical(JArray(List(value))))
    }
    val owners = JsonProtocol.deserialize(read("-reference.anno.json"))
    def isXDC(a: firrtl.annotations.Annotation): Boolean = a match {
      case _: InternalXDCAnnotation | _: XDCAnnotation => true
      case _ => false
    }
    // The companion's JSON is normalized for hashing. Its translation binding
    // also retains the ORIGINAL order, which matters to downstream XDC. Restore
    // that order rather than letting lexical JSON order become compiler policy.
    def normalized(s: String): String = s.replace(",ArraySeq(", ",List(")
    val originalOrder = (parse(read("-binding.json")) \ "annotation_translation" \ "mappings")
      .asInstanceOf[JArray].arr.map(r => (r \ "source").asInstanceOf[JString].s).filter(s =>
        s.startsWith("XDCAnnotation(") || s.startsWith("InternalXDCAnnotation(")).map(normalized)
    val bySource = owners.filter(isXDC).map(a => normalized(a.serialize) -> a).toMap
    assert(originalOrder.size == 7 && bySource.size == originalOrder.size &&
      originalOrder.distinct.size == originalOrder.size && originalOrder.forall(bySource.contains))
    val ordered = originalOrder.map(bySource).iterator
    val annotations = owners.map(a => if (isXDC(a)) ordered.next() else a) ++ contexts
    assert(!ordered.hasNext)
    val h = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm, annotations)
    assert(annotations.size > 1 && annotations.exists(_.isInstanceOf[midas.passes.ChannelClockInfoAnnotation]))
    val t = ModuleTarget("F1Shim", "F1Shim").instOf("top", "FPGATop").instOf("sim", "SimWrapper")
    val result = NativeCheckpointInPlaceContext.generate(h, t)
    NativeCheckpointInPlaceContext.validate(result, result.state)
    assert(result.native.identity.entries.size == 10151)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-inplace-rocket-production")
    saveContext(result, out)
    val constrained = WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      XDCPathToCircuitAnnotation(None, Some("shell/CL"))))
    constrained.annotations.collect { case a: XDCOutputAnnotation => a }.foreach(a =>
      Files.write(out.resolve("instrumented" + a.suffix.get), a.fileBody.getBytes(UTF_8)))
    saveAccess(result, out.resolve("access"), emitHost = false)
    saveGraft(result, out.resolve("graft"), emitHost = false)
    saveAccess(result, out.resolve("graft/original"), emitHost = false)
    println(s"NATIVE_INPLACE_ROCKET_PRODUCTION out=$out annotations=${annotations.size} complete_fpga_capture=false")
  }

  private def saveContext(result: NativeCheckpointInPlaceContext.Result, out: java.nio.file.Path): Unit = {
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    def json(as: firrtl.AnnotationSeq) = JsonProtocol.serialize(as.distinct.sortBy(_.serialize))
    save("host.fir", result.host.circuit.serialize)
    save("host-annotations.json", json(result.host.annotations))
    save("host-storage-annotations.json", json(result.host.annotations.filterNot(SimulatorBankProbeContext.accepts)))
    save("host-context-annotations.json", json(result.host.annotations.filter(SimulatorBankProbeContext.accepts)))
    save("instrumented.fir", result.state.circuit.serialize)
    save("instrumented-annotations.json", json(result.state.annotations))
    save("instrumented-storage-annotations.json", json(result.native.state.annotations))
    save("reference.fir", result.reference.circuit.serialize)
    save("reference-annotations.json", json(result.reference.annotations))
    save("binding.json", result.binding); save("compiler-context.json", result.context)
    save("native-binding.json", result.native.binding)
    save("annotation-projection.json", result.native.annotationProjection)
    save("generated-annotations.json", json(result.native.generatedAnnotations))
    save("global-identity.json", result.native.identity.manifest)
  }

  it should "instrument the existing execution and keep both ancestor control clocks raw" in {
    val (h, r) = fixture; val result = generate(h, r)
    val paths = GeneratedExternalOwners.instances(result.state, "NativeHost").map(_._1)
    assert(paths.count(_ == "NativeHost.stage.network") == 1)
    assert(!paths.exists(_.contains("nativeCapture")))
    Seq("NativeHost", "Stage").foreach { name =>
      val before = h.circuit.modules.find(_.name == name).get.asInstanceOf[Module]
      val after = result.state.circuit.modules.find(_.name == name).get.asInstanceOf[Module]
      assert(after.body.serialize.startsWith(before.body.serialize))
      assert(after.ports.filterNot(_.name.startsWith(NativeCheckpointInPlace.prefix)) == before.ports)
    }
    val out = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").map(Paths.get(_).resolve("native-inplace"))
      .getOrElse(Files.createTempDirectory("native-inplace-"))
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    save("host.fir", h.circuit.serialize); save("instrumented.fir", result.state.circuit.serialize)
    save("instrumented.sv", emit(result.state)); save("binding.json", result.binding)
    saveAnnotations(h, result, save)
    saveIdentity(result, save)
    // Retain separately reconstructed selected-subtree banks/clock provenance.
    val probe = NativeFunctionalBankProbe(r.copy(annotations = r.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(r.circuit.main),
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None))))
    probe.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }
      .foreach(a => save("FireSim-generated" + a.fileSuffix, a.body))
    val refRTL = r.copy(circuit = r.circuit.copy(main = "NativeReference", modules = r.circuit.modules.map {
      case m: Module if m.name == r.circuit.main => m.copy(name = "NativeReference")
      case m => m
    }), annotations = Nil)
    save("NativeReference.sv", emit(refRTL))
    println(s"NATIVE_INPLACE_ELABORATED out=$out existing_execution=1 raw_ancestors=2")
  }

  it should "connect functional and physical readout to the existing host on its proven raw clock" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val h = accessFixture
    val result = NativeCheckpointInPlaceContext.generate(h, target)
    val files = NativeCheckpointInPlaceAccess.emit(result, XDCPathToCircuitAnnotation(Some("host"), Some("host")))
    val rtl = files.find(_.fileSuffix == ".native-inplace-access.sv").get.body
    val binding = parse(files.find(_.fileSuffix == ".native-inplace-access.json").get.body)
    assert(rtl.contains("NativeHost network("))
    assert(rtl.contains(".checkpoint_inplace_ckpt_reset(capture_reset)"))
    assert(rtl.contains(".checkpoint_inplace_ckpt_req_valid(functional_request_valid)"))
    assert(!rtl.contains(".ckpt_clock(")) // Already connected inside the original hierarchy.
    assert((binding \ "in_place_ce_endpoint_connected") == JBool(true))
    assert((binding \ "in_place_checkpoint_widget_connected") == JBool(false))
    assert((binding \ "complete_fpga_capture") == JBool(false))
    val changedIdentity = result.native.identity.copy(entries = result.native.identity.entries.reverse)
    intercept[IllegalArgumentException](NativeCheckpointInPlaceAccess.emit(
      result.copy(native = result.native.copy(identity = changedIdentity))))
    intercept[IllegalArgumentException](NativeCheckpointInPlaceAccess.emit(
      result.copy(plan = result.plan + " ")))
    intercept[IllegalArgumentException](NativeCheckpointInPlaceAccess.emit(
      result.copy(dependencies = result.dependencies + " ")))
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-inplace-access")
    saveAccess(result, out, emitHost = true)
    println(s"NATIVE_INPLACE_ACCESS out=$out existing_execution=1 ce_endpoint_connected=true complete_fpga_capture=false")
  }

  private def saveAccess(result: NativeCheckpointInPlaceContext.Result, out: java.nio.file.Path,
      emitHost: Boolean): Unit = {
    val h = result.host.copy(annotations = result.host.annotations.filterNot(SimulatorBankProbeContext.accepts))
    val files = NativeCheckpointInPlaceAccess.emit(result, XDCPathToCircuitAnnotation(Some("host"), Some("host")))
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    save("host.fir", h.circuit.serialize); save("instrumented.fir", result.state.circuit.serialize)
    if (emitHost) save("instrumented.sv", emit(result.native.state))
    save("binding.json", result.native.binding)
    saveAnnotations(h, result.native, save); saveIdentity(result.native, save)
    saveContext(result, out.resolve("context"))
    val r = result.reference
    val probe = NativeFunctionalBankProbe(r.copy(annotations = r.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(r.circuit.main), XDCPathToCircuitAnnotation(None, None))))
    probe.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }
      .foreach(a => save("FireSim-generated" + a.fileSuffix, a.body))
    files.foreach(a => save("FireSim-generated" + a.fileSuffix, a.body))
    val refRTL = r.copy(circuit = r.circuit.copy(main = "NativeReference", modules = r.circuit.modules.map {
      case m: Module if m.name == r.circuit.main => m.copy(name = "NativeReference")
      case m => m
    }), annotations = Nil)
    save("NativeReference.sv", emit(refRTL))
  }

  private def saveGraft(result: NativeCheckpointInPlaceContext.Result, out: java.nio.file.Path,
      emitHost: Boolean): Unit = {
    val graft = NativeCheckpointInPlaceGraft.generate(result, XDCPathToCircuitAnnotation(Some("host"), Some("host")))
    val root = result.host.circuit.main
    val before = GeneratedExternalOwners.instances(result.state, root).map { case (p, m) => p -> m.name }
    val after = GeneratedExternalOwners.instances(graft.state, root).map { case (p, m) => p -> m.name }
    assert(after.filterNot(_._1 == root + ".checkpoint_readout_adapter") == before)
    assert(graft.state.annotations == result.state.annotations)
    val top = graft.state.circuit.modules.find(_.name == root).get
    assert(!top.ports.exists(_.name.startsWith(NativeCheckpointInPlace.prefix)))
    assert(top.ports.filterNot(_.name.startsWith("capture_")) ==
      result.host.circuit.modules.find(_.name == root).get.ports)
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    save("grafted.fir", graft.state.circuit.serialize)
    save("grafted-annotations.json", JsonProtocol.serialize(graft.state.annotations))
    if (emitHost) save("grafted.sv", emit(graft.state))
    graft.files.foreach(f => save("FireSim-generated" + f.fileSuffix, f.body))
  }

  it should "graft readout inside the existing root without wrapping or moving execution" in {
    val result = NativeCheckpointInPlaceContext.generate(accessFixture, target)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-inplace-graft")
    saveGraft(result, out, emitHost = true)
    // Retain original source, context, selectors and independent golden model.
    saveAccess(result, out.resolve("original"), emitHost = true)
    val graft = NativeCheckpointInPlaceGraft.generate(result)
    val adapter = graft.files.find(_.fileSuffix == ".native-inplace-graft.sv").get.body
    assert(!adapter.contains("NativeHost network("))
    assert(adapter.contains("assign checkpoint_inplace_ckpt_req_valid = functional_request_valid;"))
    assert(adapter.contains("assign capture_logical_ce = checkpoint_inplace_checkpoint_physical_logical_ce;"))
    Seq(result.copy(plan = result.plan + " "), result.copy(dependencies = result.dependencies + " "),
      result.copy(state = result.state.copy(circuit = result.state.circuit.copy(main = "Changed"))))
      .foreach(r => intercept[IllegalArgumentException](NativeCheckpointInPlaceGraft.generate(r)))
    val collision = accessFixture
    val root = collision.circuit.modules.find(_.name == collision.circuit.main).get.asInstanceOf[Module]
    Seq("capture_reset", "checkpoint_readout_adapter").foreach { name =>
      val changed = collision.copy(circuit = collision.circuit.copy(modules = collision.circuit.modules.map {
        case m if m.name == root.name => root.copy(body = Block(Seq(root.body,
          DefWire(NoInfo, name, UIntType(IntWidth(1))))))
        case m => m
      }))
      val rebuilt = NativeCheckpointInPlaceContext.generate(changed, target)
      assert(intercept[IllegalArgumentException](NativeCheckpointInPlaceGraft.generate(rebuilt))
        .getMessage.contains("reserved identifier collision"))
    }
    println(s"NATIVE_INPLACE_GRAFT out=$out existing_execution=1 constraints_relocated=false")
  }

  it should "reject stale source, ambiguous hierarchy, unknown host owners and nontransparent clocks" in {
    val (h, r) = fixture
    val stale = r.copy(circuit = r.circuit.copy(modules = r.circuit.modules.map {
      case m: Module if m.name == "NativeStorage" => m.copy(info = FileInfo("stale"))
      case m => m
    }))
    assert(intercept[IllegalArgumentException](generate(h, stale)).getMessage.contains("stale or different"))
    case object UnknownOwner extends NoTargetAnnotation
    assert(intercept[IllegalArgumentException](generate(h.copy(annotations = h.annotations :+ UnknownOwner), r))
      .getMessage.contains("explicit clone/rename translation"))
    assert(intercept[IllegalArgumentException](generate(h, r,
      ModuleTarget("NativeHost", "NativeHost").instOf("missing", "Stage").instOf("network", "NativeStorage")))
      .getMessage.contains("instance path mismatch"))
    def modifyStage(f: Module => Module) = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case m: Module if m.name == "Stage" => f(m)
      case m => m
    }))
    val shared = modifyStage(m => m.copy(body = Block(Seq(m.body,
      firrtl.WDefInstance("second", "NativeStorage")))))
    assert(intercept[IllegalArgumentException](generate(shared, r)).getMessage.contains("exactly one concrete execution"))
    val collision = modifyStage(m => m.copy(ports = m.ports :+
      Port(NoInfo, "checkpoint_inplace_reserved", Input, UIntType(IntWidth(1)))))
    assert(intercept[IllegalArgumentException](generate(collision, r)).getMessage.contains("reserved sideband"))
    def clocks(s: Statement): Statement = s match {
      case c: Connect if c.loc.serialize == "network.clock" =>
        c.copy(expr = DoPrim(firrtl.PrimOps.AsClock, Seq(WRef("reset", UIntType(IntWidth(1)))), Nil, ClockType))
      case x => x.mapStmt(clocks)
    }
    val gated = modifyStage(m => m.copy(body = clocks(m.body)))
    assert(intercept[IllegalArgumentException](generate(gated, r)).getMessage.contains("nontransparent raw clock"))
  }

  it should "instrument the exact Rocket SimWrapper execution without masking its enclosing FPGATop" in {
    val source = Paths.get(sys.props.getOrElse("checkpoint.inplace.rocket.reference",
      fail("checkpoint.inplace.rocket.reference must identify the retained native exact-recipe source")))
    // The .fir input is deliberately structural evidence. It carries no
    // production annotation/host-owner projection and grants no image admission.
    val text = new String(Files.readAllBytes(source), UTF_8)
    val working = firrtl.passes.ToWorkingIR.run(Parser.parse(text))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val h = CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm,
      Seq(firrtl.transforms.NoCircuitDedupAnnotation))
    val selected = GeneratedExternalOwners.instances(h, "SimWrapper").map(_._2.name).toSet
    val r = h.copy(circuit = h.circuit.copy(main = "SimWrapper", modules = h.circuit.modules.filter(m => selected(m.name))))
    val m = SimulatorStateInventory.discover(r); val p = PhysicalClockCutPlan.discover(r, m)
    val d = PhysicalClockCutDependencies.discover(r, m, p)
    val t = ModuleTarget("F1Shim", "F1Shim").instOf("top", "FPGATop").instOf("sim", "SimWrapper")
    val result = NativeCheckpointInPlace.generate(h, t, r, m, p, d)
    val identityDoc = org.json4s.native.JsonMethods.parse(result.identity.manifest)
    val identityRows = (identityDoc \ "state").asInstanceOf[org.json4s.JArray].arr
    val ceRows = identityRows.filter(row => (row \ "domain") == org.json4s.JString("physical_ce"))
    assert(ceRows.size == 2 && ceRows.forall(row => (row \ "ce_bit") != (row \ "global_physical_index")))
    val uncoveredCE = (identityDoc \ "uncovered_physical_gates").asInstanceOf[org.json4s.JArray].arr
    assert(uncoveredCE.size == 1 && (uncoveredCE.head \ "hierarchy") ==
      org.json4s.JString("F1Shim.top.FASEDMemoryTimingModel_0.gate"))
    val live = GeneratedExternalOwners.instances(result.state, "F1Shim")
    assert(live.count(_._1 == "F1Shim.top.sim") == 1)
    assert(live.exists(_._1 == "F1Shim.top.FASEDMemoryTimingModel_0.gate"))
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("native-inplace-rocket")
    Files.createDirectories(out)
    def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
    save("host.fir", h.circuit.serialize); save("instrumented.fir", result.state.circuit.serialize)
    save("source-input.json", s"""{"path":"${source.toAbsolutePath}","sha256":"${SemanticStateInventory.sha256(text)}","scope":"retained exact-recipe structural input; no production annotations or capture admission"}""")
    save("binding.json", result.binding)
    saveAnnotations(h, result, save)
    saveIdentity(result, save)
    val probe = NativeFunctionalBankProbe(r.copy(annotations = r.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(r.circuit.main),
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None))))
    probe.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }
      .foreach(a => save("FireSim-generated" + a.fileSuffix, a.body))
    println(s"NATIVE_INPLACE_ROCKET_STRUCTURAL out=$out selected=F1Shim.top.sim complete_fpga_capture=false")
  }

  private def saveIdentity(result: NativeCheckpointInPlace.Result, save: (String, String) => Unit): Unit = {
    val identity = result.identity
    save("global-identity.json", identity.manifest); save("global-identity-binding.json", identity.binding)
    save("global-simulator-state.json", identity.simulatorManifest)
    save("global-physical-clock-plan.json", identity.clockPlan)
    save("global-physical-clock-dependencies.json", identity.dependencies)
  }

  private def saveAnnotations(h: CircuitState, result: NativeCheckpointInPlace.Result,
      save: (String, String) => Unit): Unit = {
    def json(as: firrtl.AnnotationSeq) = JsonProtocol.serialize(as.distinct.sortBy(_.serialize))
    save("host-annotations.json", json(h.annotations))
    save("instrumented-annotations.json", json(result.state.annotations))
    save("generated-annotations.json", json(result.generatedAnnotations))
    save("annotation-projection.json", result.annotationProjection)
    WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      XDCPathToCircuitAnnotation(None, Some("shell/CL")))).annotations.collect {
      case a: XDCOutputAnnotation => a
    }.foreach(a => save("instrumented" + a.suffix.get, a.fileBody))
  }

  it should "project correlated host constraints and preservation into actual specialized executions" in {
    val (h, r) = fixture
    val touch = firrtl.transforms.DontTouchAnnotation(ModuleTarget("NativeHost", "Leaf").ref("value"))
    val result = generate(h.copy(annotations = h.annotations :+ touch), r)
    val touched = result.state.annotations.collect { case a: firrtl.transforms.DontTouchAnnotation => a.target }
    assert(touched.size == 2 && touched.forall(_.module != "Leaf"))
    assert(touched.map(_.module).distinct.size == 2 && touched.forall(_.circuit == "NativeHost"))
    val constraints = WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      XDCPathToCircuitAnnotation(None, Some("shell/CL")))).annotations.collect {
      case a: XDCOutputAnnotation => a.fileBody
    }.mkString("\n")
    assert(constraints.contains("-from [get_cells shell/CL/host_ticks] -to [get_cells shell/CL/stage/network/cycles]"))
    Seq("left_", "right_").foreach(i => assert(constraints.contains(s"shell/CL/stage/network/$i/gate/CE")))
    assert(constraints.contains("stage/network/ram"))
    assert(!constraints.contains("NativeStorage") && !constraints.contains("Leaf"))
    assert(!result.binding.contains("\"complete_fpga_capture\":true"))
  }

  it should "reject stale, foreign, cross-root and hidden-pin host constraints before emission" in {
    val (h, r) = fixture
    def reject(a: firrtl.annotations.Annotation, message: String): Unit = {
      assert(intercept[IllegalArgumentException](generate(h.copy(annotations = h.annotations :+ a), r))
        .getMessage.contains(message))
    }
    val top = ModuleTarget("NativeHost", "NativeHost")
    reject(firrtl.transforms.DontTouchAnnotation(top.ref("missing")), "declaration/kind mismatch")
    reject(XDCAnnotation(XDCFiles.Implementation, "set_false_path -to [get_pins {}]",
      ModuleTarget("Foreign", "NativeHost").ref("clock")), "foreign XDC")
    reject(XDCAnnotation(XDCFiles.Implementation, "set_max_delay 4 -from {} -to {}",
      top.ref("host_ticks"), ModuleTarget("NativeHost", "Stage").ref("control_ticks")), "common explicit root")
    reject(XDCAnnotation(XDCFiles.Implementation, "set_false_path -to {}",
      ModuleTarget("NativeHost", "Native").ref("hidden_latch")), "exact ground declaration")
  }

  it should "preserve an unbanked sibling sharing the selected leaf and constrain each execution exactly once" in {
    val (h, r) = fixture
    val additions = Parser.parse("""circuit Extra :
  module Extra :
    inst observer of Leaf
    observer.clock <= clock
    observer.reset <= reset
    observer.enable <= UInt<1>(1)
""").modules.head.asInstanceOf[Module].body
    val circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case m: Module if m.name == "NativeHost" => m.copy(body = Block(Seq(m.body, additions)))
      case m => m
    })
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val enclosing = h.copy(circuit = firrtl.passes.ResolveFlows.run(typed))
    val result = generate(enclosing, r)
    assert(result.state.circuit.modules.find(_.name == "Leaf") == h.circuit.modules.find(_.name == "Leaf"))
    val constraints = WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      XDCPathToCircuitAnnotation(None, None))).annotations.collect {
      case a: XDCOutputAnnotation => a.fileBody
    }.mkString("\n")
    Seq("observer/gate/CE", "stage/network/left_/gate/CE", "stage/network/right_/gate/CE").foreach { path =>
      assert(constraints.sliding(path.length).count(_ == path) == 1)
    }
    val clocks = constraints.linesIterator.filter(_.startsWith("set_max_delay 9")).toSeq
    assert(clocks.size == 3 && !clocks.exists(_.contains("checkpoint_physical_root_gate")))
    Seq("observer/gate", "stage/network/left_/gate", "stage/network/right_/gate").foreach { path =>
      assert(clocks.count(_ == s"set_max_delay 9 -from [get_pins $path/I] -to [get_pins $path/O]") == 1)
    }
  }

  it should "keep selected primitive constraints on original gates after adding the mask gate" in {
    val (h, original) = fixture
    val constraint = XDCAnnotation(XDCFiles.Implementation,
      "set_max_delay 11 -from [get_pins {}] -to [get_pins {}]",
      ModuleTarget("NativeStorage", "Native").ref("I"), ModuleTarget("NativeStorage", "Native").ref("O"))
    val r = original.copy(annotations = original.annotations :+ constraint)
    val result = generate(h, r)
    val lines = WriteXDCFile.execute(result.state.copy(annotations = result.state.annotations :+
      XDCPathToCircuitAnnotation(None, Some("shell/CL")))).annotations.collect {
      case a: XDCOutputAnnotation => a.fileBody
    }.mkString("\n").linesIterator.filter(_.startsWith("set_max_delay 11")).toSeq
    assert(lines.size == 2 && !lines.exists(_.contains("checkpoint_physical_root_gate")))
    Seq("left_", "right_").foreach { side =>
      val path = s"shell/CL/stage/network/$side/gate"
      assert(lines.contains(s"set_max_delay 11 -from [get_pins $path/I] -to [get_pins $path/O]"))
    }
  }

  it should "use original global identities while preserving local selectors and declaring every uncovered owner" in {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse
    val (h, r) = fixture
    val m = SimulatorStateInventory.discover(r); val p = PhysicalClockCutPlan.discover(r, m)
    val d = PhysicalClockCutDependencies.discover(r, m, p)
    val result = generate(h, r)
    val identity = result.identity
    val global = (parse(identity.simulatorManifest) \ "state").asInstanceOf[JArray].arr
      .filterNot(row => (row \ "adapter") == JString("metasim-bufgce-v1"))
    val projection = parse(identity.manifest)
    val rows = (projection \ "state").asInstanceOf[JArray].arr
    val uncovered = (projection \ "uncovered_functional_state").asInstanceOf[JArray].arr
    assert(uncovered.map(_ \ "hierarchy").toSet == Set(JString("NativeHost.host_ticks"),
      JString("NativeHost.stage.control_ticks")))
    assert(global.map(_ \ "state_id").toSet ==
      (rows.filter(row => (row \ "domain") == JString("functional")).map(_ \ "state_id") ++
        uncovered.map(_ \ "state_id")).toSet)
    val local = midas.widgets.CheckpointCaptureDescriptors.fromNativeGenerated(
      GeneratedStateBankAccess.generatePhysicalCompanion(r, m, p, d), r, m, p, d)
    assert(identity.entries.map(e => (e.bank, e.slot, e.width, e.depth)) ==
      local.entries.map(e => (e.bank, e.slot, e.width, e.depth)))
    assert(identity.entries.map(_.stateID).toSet.intersect(local.entries.map(_.stateID).toSet).isEmpty)
    val descriptors = midas.widgets.CheckpointCaptureDescriptors.fromNativeInPlace(h, target, r, m, p, d)
    assert(descriptors.entries == identity.entries && descriptors.manifest == identity.manifest)
    assert(descriptors.manifestSha256 == SemanticStateInventory.sha256(identity.manifest))
    assert(descriptors.cppManifest.contains("255U"))
    assert((projection \ "complete_fpga_capture") == JBool(false))
    assert((projection \ "metasim_restore_supported") == JBool(false))
    assert((parse(result.binding) \ "identity_binding_sha256") ==
      JString(SemanticStateInventory.sha256(identity.binding)))
  }

  it should "reject stale concrete paths, changed original state and stale selected provenance during identity reconciliation" in {
    val (h, r) = fixture
    val m = SimulatorStateInventory.discover(r); val p = PhysicalClockCutPlan.discover(r, m)
    val d = PhysicalClockCutDependencies.discover(r, m, p)
    def project(host: CircuitState = h, t: InstanceTarget = target, manifest: String = m) =
      NativeCheckpointIdentity.discover(host, t, r, manifest, p, d)
    val wrong = ModuleTarget("NativeHost", "NativeHost").instOf("stage", "Stage")
      .instOf("missing", "NativeStorage")
    assert(intercept[IllegalArgumentException](project(t = wrong)).getMessage.contains("stale concrete target"))
    assert(intercept[IllegalArgumentException](project(manifest = m.replace("firrtl-memory-v1", "unsupported")))
      .getMessage.contains("stale or incomplete simulator inventory"))
    val changed = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case mod: Module if mod.name == "NativeStorage" => mod.copy(info = FileInfo("changed"))
      case mod => mod
    }))
    assert(intercept[IllegalArgumentException](project(host = changed)).getMessage.contains("stale executing"))
  }
}
