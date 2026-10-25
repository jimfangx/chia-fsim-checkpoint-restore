// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.ir.{ExtModule, Module}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeFunctionalBankSpec extends AnyFlatSpec {
  private val source = """circuit NativeStorage :
  extmodule Native :
    input I : Clock
    input CE : UInt<1>
    output O : Clock
    defname = BUFGCE
  module Leaf :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<1>
    output count : UInt<65>
    inst gate of Native
    gate.I <= clock
    gate.CE <= enable
    reg value : UInt<65>, gate.O with :
      reset => (reset, UInt<65>(0))
    value <= add(value, UInt<1>(1))
    count <= value
  module NativeStorage :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<2>
    input address : UInt<3>
    input data : UInt<130>
    input write : UInt<1>
    output left_count : UInt<65>
    output right_count : UInt<65>
    output cycle : UInt<64>
    output read_data : UInt<130>
    reg cycles : UInt<64>, clock with :
      reset => (reset, UInt<64>(0))
    cycles <= add(cycles, UInt<1>(1))
    cycle <= cycles
    inst left of Leaf
    inst right of Leaf
    left.clock <= clock
    right.clock <= clock
    left.reset <= reset
    right.reset <= reset
    left.enable <= bits(enable, 0, 0)
    right.enable <= bits(enable, 1, 1)
    left_count <= left.count
    right_count <= right.count
    mem ram :
      data-type => UInt<130>
      depth => 8
      reader => r
      writer => w
      read-latency => 1
      write-latency => 1
      read-under-write => old
    ram.r.clk <= clock
    ram.r.en <= UInt<1>(1)
    ram.r.addr <= address
    ram.w.clk <= clock
    ram.w.en <= write
    ram.w.addr <= address
    ram.w.data <= data
    ram.w.mask <= UInt<1>(1)
    read_data <= ram.r.data
"""
  private def input = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(source),
    ChirrtlForm, Seq(firrtl.transforms.NoCircuitDedupAnnotation))).copy(form = LowForm)
  private val out = sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").map(Paths.get(_).resolve("native-functional"))
    .getOrElse(Files.createTempDirectory("native-functional-"))
  Files.createDirectories(out)
  private def save(name: String, body: String): Unit = Files.write(out.resolve(name), body.getBytes(UTF_8))
  private def emit(s: CircuitState): String = {
    val emitter = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
      .getEmittedCircuit.value
  }

  behavior.of("Native functional storage readout companion")
  it should "retain exact native leaves and every concrete storage word with an independent endpoint clock" in {
    val s = input
    val manifest = SimulatorStateInventory.discover(s)
    val plan = PhysicalClockCutPlan.discover(s, manifest)
    val deps = PhysicalClockCutDependencies.discover(s, manifest, plan)
    val inventory = GeneratedStateBankAccess.physicalFunctionalInventory(s, manifest, plan, deps)
    val result = GeneratedStateBankAccess.generatePhysicalCompanion(s, manifest, plan, deps)
    GeneratedStateBankAccess.validateAnnotationBinding(result)
    val rows = (parse(inventory) \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 5 && result.entries.size == 5)
    assert(rows.forall(r => (r \ "adapter") != JString("metasim-bufgce-v1")))
    assert(result.state.circuit.modules.collect { case m: ExtModule => m } ==
      s.circuit.modules.collect { case m: ExtModule => m })
    assert((parse(result.binding) \ "scope") == JString("native_functional_readout_companion_only"))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((parse(result.binding) \ k) == JBool(false)))
    // Neither legacy descriptor admission nor the ordinary bank path is widened.
    intercept[IllegalArgumentException] { midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, inventory) }
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(s, manifest, separateAccessClock = true)
    }.getMessage.contains("requires a synthesized capture owner"))
    save("reference.fir", s.circuit.serialize); save("manifest.json", manifest)
    save("physical-clock-plan.json", plan); save("physical-clock-dependencies.json", deps)
    save("functional-inventory.json", inventory); save("binding.json", result.binding)
    save("instrumented.fir", result.state.circuit.serialize); save("instrumented.sv", emit(result.state))
    val reference = s.copy(circuit = s.circuit.copy(main = "NativeReference", modules = s.circuit.modules.map {
      case m: Module if m.name == s.circuit.main => m.copy(name = "NativeReference")
      case m => m
    }), annotations = Nil)
    save("reference.sv", emit(reference))
    println(s"NATIVE_FUNCTIONAL_BANK_ELABORATED out=$out entries=${rows.size}")
  }

  it should "reject stale inventories, CE dependencies, topology, and coherently changed native definitions" in {
    val s = input
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    Seq((m + " ", p, d), (m, p + " ", d), (m, p, d + " ")).foreach { case (mm, pp, dd) =>
      intercept[IllegalArgumentException] { GeneratedStateBankAccess.generatePhysicalCompanion(s, mm, pp, dd) }
    }
    val altered = s.copy(circuit = s.circuit.copy(modules = s.circuit.modules.map {
      case e: ExtModule => e.copy(params = Seq(firrtl.ir.IntParam("IS_I_INVERTED", 1)))
      case x => x
    }))
    intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generatePhysicalCompanion(altered, m, p, d)
    }
    val good = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val bad = good.copy(binding = good.binding.replace("NativeStorage.left_.gate", "NativeStorage.missing.gate"))
    intercept[IllegalArgumentException] { GeneratedStateBankAccess.validateAnnotationBinding(bad) }
    val fake = good.copy(binding = good.binding.replace("\"complete_fpga_capture\":false", "\"complete_fpga_capture\":true"))
    intercept[IllegalArgumentException] { GeneratedStateBankAccess.validateAnnotationBinding(fake) }
  }
  it should "emit a separate sibling only under an explicit whole-root native probe request" in {
    val original = input
    val requested = original.copy(annotations = original.annotations ++ Seq(
      NativeFunctionalBankProbeAnnotation, PhysicalClockCutPlanAnnotation,
      SimulatorCheckpointInventoryAnnotation(original.circuit.main),
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None),
      midas.InternalXDCAnnotation(midas.targetutils.xdc.XDCFiles.Synthesis,
        "set_property DONT_TOUCH true [get_cells {}]",
        firrtl.annotations.ModuleTarget("NativeStorage", "Leaf").ref("gate"))))
    val result = NativeFunctionalBankProbe(requested)
    assert(result.circuit == original.circuit)
    val files = result.annotations.collect { case a: midas.stage.GoldenGateOutputFileAnnotation => a }
    assert(files.size == 29 && files.map(_.fileSuffix).distinct.size == 29)
    val receipt = parse(files.find(_.fileSuffix == ".native-functional-emission.json").get.body)
    assert((receipt \ "deployable_capture_image") == JBool(false))
    files.filter(f => f.fileSuffix.startsWith(".native-functional") && f.fileSuffix != ".native-functional-emission.json").foreach { f =>
      assert((receipt \ "inputs" \ f.fileSuffix) == JString(SemanticStateInventory.sha256(f.body)))
    }
    val descriptors = parse(files.find(_.fileSuffix == ".native-descriptors-emission.json").get.body)
    assert((descriptors \ "same_reference_sha256") == JString(SemanticStateInventory.sha256(original.circuit.serialize)))
    files.filter(f => f.fileSuffix.startsWith(".native-descriptors") &&
      f.fileSuffix != ".native-descriptors-emission.json").foreach { f =>
      assert((descriptors \ "inputs" \ f.fileSuffix) == JString(SemanticStateInventory.sha256(f.body)))
    }
    assert((descriptors \ "inputs" \ ".native-functional-emission.json") ==
      JString(SemanticStateInventory.sha256(files.find(_.fileSuffix == ".native-functional-emission.json").get.body)))
    val wiring = parse(files.find(_.fileSuffix == ".native-clock-cut-emission.json").get.body)
    files.filter(f => f.fileSuffix.startsWith(".native-clock-cut") &&
      f.fileSuffix != ".native-clock-cut-emission.json").foreach { f =>
      assert((wiring \ "inputs" \ f.fileSuffix) == JString(SemanticStateInventory.sha256(f.body)))
    }
    intercept[IllegalArgumentException] { NativeFunctionalBankProbe(result) }
    Seq(".native-descriptors.json", ".native-descriptors-host.inc", ".native-descriptors-emission.json",
      ".native-clock-cut.sv", ".native-clock-cut-emission.json", ".native-state-access.sv",
      ".native-state-access-router.sv", ".native-state-access.json").foreach { suffix =>
      intercept[IllegalArgumentException] { NativeFunctionalBankProbe(requested.copy(annotations =
        requested.annotations :+ midas.stage.GoldenGateOutputFileAnnotation("existing", suffix))) }
    }
    Seq(PhysicalClockCutPlanAnnotation, SimulatorCheckpointInventoryAnnotation(original.circuit.main)).foreach { missing =>
      intercept[IllegalArgumentException] { NativeFunctionalBankProbe(requested.copy(
        annotations = requested.annotations.filterNot(_ == missing))) }
    }
    intercept[IllegalArgumentException] { NativeFunctionalBankProbe(requested.copy(
      annotations = requested.annotations :+ SimulatorBankProbeAnnotation)) }
  }

  it should "connect the exact native banks and physical CE endpoint through one raw-clock router" in {
    val s = input
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    val b = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val w = PhysicalClockCutWiring.generateNative(s, m, p, d, b)
    val files = NativeCheckpointStateAccess.emit(s, m, p, d, b, w)
    val rtl = files.head.body
    val binding = parse(files.find(_.fileSuffix == ".native-state-access.json").get.body)
    assert(rtl.contains("NativeStorage network(") && rtl.contains("NativeStorageCheckpointPhysicalCEBank ce_endpoint("))
    assert(rtl.contains(".ckpt_clock(clock),.ckpt_reset(capture_reset),.ckpt_freeze(capture_clock_mask)"))
    assert(rtl.contains(".clock(clock),.reset(capture_reset),.read_enable(capture_read_enable)"))
    assert((binding \ "functional_entries") == JInt(5) && (binding \ "physical_clock_gates") == JInt(2))
    assert((binding \ "endpoint_sha256") == JString(SemanticStateInventory.sha256(rtl)))
    assert((binding \ "router_sha256") == JString(SemanticStateInventory.sha256(files(1).body)))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").foreach(k => assert((binding \ k) == JBool(false)))
  }

  it should "reject changed clock-mask circuits and coherently rehashed admission bindings before routing" in {
    val s = input
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    val b = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val w = PhysicalClockCutWiring.generateNative(s, m, p, d, b)
    val changed = w.state.copy(circuit = w.state.circuit.copy(info = firrtl.ir.FileInfo("changed")))
    Seq(w.copy(state = changed), w.copy(binding = w.binding + " ")).foreach { bad =>
      intercept[IllegalArgumentException] { NativeCheckpointStateAccess.emit(s, m, p, d, b, bad) }
    }
  }

  it should "preserve correlated native pin constraints through real storage specialization" in {
    import firrtl.annotations.ModuleTarget
    import midas.InternalXDCAnnotation
    import midas.targetutils.xdc.{XDCFiles, XDCPathToCircuitAnnotation}
    val original = input
    val pin = ModuleTarget("NativeStorage", "Leaf").instOf("gate", "Native").ref("O")
    val constraint = InternalXDCAnnotation(XDCFiles.Implementation,
      "set_max_delay 7.0 -from [get_pins {}]", pin)
    val shell = InternalXDCAnnotation(XDCFiles.Synthesis,
      "set_property IOSTANDARD LVDS [get_ports shell_clock]")
    val s = original.copy(annotations = original.annotations ++ Seq(constraint, shell))
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    val generated = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val constraints = generated.state.annotations.collect { case a: InternalXDCAnnotation => a }
    val targeted = constraints.filter(_.argumentList.nonEmpty)
    assert(targeted.size == 2 && targeted.map(_.argumentList.head.module).distinct.size == 2)
    assert(constraints.filter(_.argumentList.isEmpty) == Seq(shell))
    val emitted = midas.passes.WriteXDCFile.execute(generated.state.copy(annotations =
      generated.state.annotations :+ XDCPathToCircuitAnnotation(None, None)))
    val xdc = emitted.annotations.collect { case a: midas.passes.XDCOutputAnnotation => a.fileBody }.mkString
    assert(xdc.contains("left_/gate/O") && xdc.contains("right_/gate/O"))
    GeneratedStateBankAccess.validateAnnotationBinding(generated)
    val wired = PhysicalClockCutWiring.generateNative(s, m, p, d, generated)
    val wiredXDC = midas.passes.WriteXDCFile.execute(wired.state.copy(annotations =
      wired.state.annotations :+ XDCPathToCircuitAnnotation(None, None)))
      .annotations.collect { case a: midas.passes.XDCOutputAnnotation => a.fileBody }.mkString
    assert(wiredXDC == xdc)
    val rtl = emit(wired.state)
    assert(rtl.contains("assign gate_I = checkpoint_physical_source_clock;"))
    assert(rtl.contains("always @(posedge ckpt_clock)"))
    assert((parse(wired.binding) \ "bank_annotations_preserved") == JBool(true))
    val files = NativeCheckpointStateAccess.emit(s, m, p, d, generated, wired,
      XDCPathToCircuitAnnotation(Some("shell/pre"), Some("shell/post")))
    val wrapped = files.find(_.fileSuffix == ".native-state-access.implementation.xdc").get.body
    assert(wrapped.contains("shell/post/network/left_/gate/O") &&
      wrapped.contains("shell/post/network/right_/gate/O"))
    assert(!wrapped.contains("shell/pre/network/"))
    val synthesis = files.find(_.fileSuffix == ".native-state-access.synthesis.xdc").get.body
    assert(synthesis.endsWith(shell.formatString) && !synthesis.contains("network/shell_clock"))
    val binding = parse(files.find(_.fileSuffix == ".native-state-access-constraints.json").get.body)
    assert((binding \ "rebased_pre_link_path") == JString("shell/pre/network"))
    assert((binding \ "production_constraints_attached") == JBool(false))
  }

  it should "reject unsafe constraint placements and stale constraint targets before emission" in {
    import firrtl.annotations.ModuleTarget
    import midas.InternalXDCAnnotation
    import midas.targetutils.xdc.{XDCFiles, XDCPathToCircuitAnnotation}
    val s = input
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    val b = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val w = PhysicalClockCutWiring.generateNative(s, m, p, d, b)
    Seq("", "/shell", "shell/", "shell//top", "shell/../top", "shell/*", "shell/[bad]", "shell/$x").foreach { bad =>
      // A literal dollar is a legal HDL identifier character; Tcl substitution
      // requires a stricter placement contract than ordinary Verilog names.
      intercept[IllegalArgumentException] {
        NativeCheckpointConstraints.emit(w.state, s.circuit.main + "CheckpointStateAccess",
          XDCPathToCircuitAnnotation(Some(bad), None))
      }
    }
    val stale = InternalXDCAnnotation(XDCFiles.Implementation, "set_max_delay 7 -from [get_pins {}]",
      ModuleTarget(s.circuit.main, "MissingModule").ref("missing"))
    intercept[IllegalArgumentException] {
      NativeCheckpointConstraints.emit(w.state.copy(annotations = w.state.annotations :+ stale),
        s.circuit.main + "CheckpointStateAccess", XDCPathToCircuitAnnotation(None, None))
    }
    intercept[IllegalArgumentException] {
      NativeCheckpointConstraints.emit(w.state, "ChangedWrapper", XDCPathToCircuitAnnotation(None, None))
    }
  }

  it should "reject native wiring with coherently changed banks, selectors or source identity" in {
    val s = input
    val m = SimulatorStateInventory.discover(s); val p = PhysicalClockCutPlan.discover(s, m)
    val d = PhysicalClockCutDependencies.discover(s, m, p)
    val good = GeneratedStateBankAccess.generatePhysicalCompanion(s, m, p, d)
    val changed = good.state.copy(circuit = good.state.circuit.copy(info = firrtl.ir.FileInfo("changed")))
    val binding = SemanticStateInventory.canonical(parse(good.binding) merge JObject(List(
      "instrumented_sha256" -> JString(SemanticStateInventory.sha256(changed.circuit.serialize)))))
    Seq(good.copy(state = changed, binding = binding),
      good.copy(entries = good.entries.updated(0, good.entries.head.copy(slot = 12))),
      good.copy(binding = good.binding.replace("\"complete_fpga_capture\":false", "\"complete_fpga_capture\":true")))
      .foreach { bad => intercept[IllegalArgumentException] {
        PhysicalClockCutWiring.generateNative(s, m, p, d, bad)
      }}
    intercept[IllegalArgumentException] {
      PhysicalClockCutWiring.generateNative(s, m, p, d + " ", good)
    }
  }

}
