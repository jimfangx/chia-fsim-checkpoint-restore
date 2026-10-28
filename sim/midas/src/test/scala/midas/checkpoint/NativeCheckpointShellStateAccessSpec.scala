// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser}
import firrtl.annotations.{JsonProtocol, ModuleTarget, NoTargetAnnotation}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

case class UnknownShellOwnerAnnotation() extends NoTargetAnnotation

class NativeCheckpointShellStateAccessSpec extends AnyFlatSpec {
  private val source = """circuit ShellTop :
  module Child :
    input clock : Clock
    input reset : UInt<1>
    output live : UInt<8>
    reg count : UInt<8>, clock with :
      reset => (reset, UInt<8>(0))
    count <= add(count, UInt<8>(1))
    live <= count
  module Shell :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    input writeEn : UInt<1>
    input address : UInt<2>
    output value : UInt<65>
    output cycle : UInt<8>
    output memoryOut : UInt<8>
    output childLive : UInt<8>
    inst child of Child
    child.clock <= clock
    child.reset <= reset
    childLive <= child.live
    reg saved : UInt<65>, clock with :
      reset => (reset, UInt<65>(0))
    reg cycles : UInt<8>, clock with :
      reset => (reset, UInt<8>(0))
    saved <= add(saved, UInt<65>(18446744073709551619))
    cycles <= add(cycles, UInt<8>(1))
    value <= saved
    cycle <= cycles
    mem mem :
      data-type => UInt<8>
      depth => 4
      read-latency => 1
      write-latency => 1
      reader => r
      writer => w
      read-under-write => old
    mem.r.clk <= clock
    mem.r.en <= UInt<1>(1)
    mem.r.addr <= address
    memoryOut <= mem.r.data
    mem.w.clk <= clock
    mem.w.en <= writeEn
    mem.w.addr <= address
    mem.w.data <= data
    mem.w.mask <= UInt<1>(1)
  module ShellTop :
    input clock : Clock
    input reset : UInt<1>
    input data : UInt<8>
    input writeEn : UInt<1>
    input address : UInt<2>
    output value : UInt<65>
    output cycle : UInt<8>
    output memoryOut : UInt<8>
    output childLive : UInt<8>
    output siblingLive : UInt<8>
    inst enclosing of Shell
    inst sibling of Child
    enclosing.clock <= clock
    enclosing.reset <= reset
    enclosing.data <= data
    enclosing.writeEn <= writeEn
    enclosing.address <= address
    value <= enclosing.value
    cycle <= enclosing.cycle
    memoryOut <= enclosing.memoryOut
    childLive <= enclosing.childLive
    sibling.clock <= clock
    sibling.reset <= reset
    siblingLive <= sibling.live
"""
  private def lower(s: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(s), ChirrtlForm)).copy(form = LowForm)
  private def selected = ModuleTarget("ShellTop", "ShellTop").instOf("enclosing", "Shell")
  private def generated = NativeCheckpointShellStateAccess.generate(lower(), selected)
  private def module(s: CircuitState, name: String): Module = s.circuit.modules.find(_.name == name).get.asInstanceOf[Module]
  private def emit(s: CircuitState): String = {
    val result = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
    result.annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
  }
  private def saveArtifacts(result: NativeCheckpointShellStateAccess.Result): Unit = {
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { root =>
      val dir = java.nio.file.Paths.get(root, "shell-state-access")
      java.nio.file.Files.createDirectories(dir)
      def save(name: String, value: String): Unit = { java.nio.file.Files.write(dir.resolve(name),
        value.getBytes(java.nio.charset.StandardCharsets.UTF_8)); () }
      save("source.fir", result.host.circuit.serialize)
      save("source.sv", emit(result.host))
      save("source.anno.json", JsonProtocol.serialize(result.host.annotations))
      save("reference.fir", result.reference.circuit.serialize)
      save("reference.anno.json", JsonProtocol.serialize(result.reference.annotations))
      save("generated.fir", result.state.circuit.serialize)
      save("generated.sv", emit(result.state))
      save("generated.anno.json", JsonProtocol.serialize(result.state.annotations))
      save("manifest.json", result.manifest)
      save("mapping.json", result.mapping)
      save("binding.json", result.binding)
      save("banks-binding.json", result.banks.binding)
    }
  }

  behavior.of("Existing enclosing-module local state access")
  it should "bank every local register, wide word, memory and synchronous pipeline without banking children" in {
    val result = generated
    assert(result.entries.size == 4)
    val rows = (parse(result.mapping) \ "state").asInstanceOf[JArray].arr
    assert(rows.forall(r => (r \ "hierarchy").values.toString.startsWith("ShellTop.enclosing.")))
    assert(!rows.exists(r => (r \ "hierarchy").values.toString.contains(".child.")))
    assert(rows.exists(r => (r \ "hierarchy") == JString("ShellTop.enclosing.mem_r_data")))
    assert(rows.exists(r => (r \ "storage_hierarchy") == JString("ShellTop.enclosing.CheckpointPipeline_mem_r_data")))
    assert(result.entries.exists(e => e.width == 64 && e.depth == 2))
    assert(result.reference.circuit.modules.size == 1)
    assert(!result.reference.circuit.serialize.contains("inst child"))
    assert(result.reference.circuit.serialize.contains("checkpoint_shell_boundary_child_live"))
    assert(!result.state.circuit.serialize.contains("checkpoint_shell_boundary_"))
    assert(module(result.state, "Shell").ports.take(module(result.host, "Shell").ports.size) == module(result.host, "Shell").ports)
    assert(module(result.state, "Child") == module(result.host, "Child"))
    assert(GeneratedExternalOwners.instances(result.state, "ShellTop").map { case (p, m) => p -> m.name } ==
      GeneratedExternalOwners.instances(result.host, "ShellTop").map { case (p, m) => p -> m.name })
    NativeCheckpointShellStateAccess.validate(result)
    saveArtifacts(result)
  }

  it should "preserve the child execution and route independent access sidebands through original ancestors" in {
    val result = generated
    val shell = module(result.state, "Shell").body.serialize
    assert(shell.contains("child.clock <= clock") && shell.contains("child.reset <= reset"))
    assert(shell.contains("childLive <= child.live"))
    assert(!shell.contains("child.ckpt_") && !shell.contains("child.checkpoint_shell_"))
    assert(shell.contains("mux(checkpoint_shell_ckpt_freeze, saved"))
    assert(shell.contains("mux(checkpoint_shell_ckpt_freeze, cycles"))
    assert(shell.contains("checkpoint_shell_ckpt_saved_data"))
    val top = module(result.state, "ShellTop").body.serialize
    assert(top.contains("enclosing.checkpoint_shell_ckpt_clock <= checkpoint_shell_ckpt_clock"))
    assert(top.contains("sibling.clock <= clock"))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported", "transport_state_supported", "child_executions_frozen").foreach { k =>
      assert((parse(result.binding) \ k) == JBool(false))
    }
  }

  it should "select a unique module directly and support a shell at the circuit root" in {
    val state = lower()
    val byModule = NativeCheckpointShellStateAccess.generate(state, ModuleTarget("ShellTop", "Shell"))
    assert(byModule.mapping == generated.mapping)
    val rootSource = source.replace("circuit ShellTop :", "circuit Shell :")
      .substring(0, source.replace("circuit ShellTop :", "circuit Shell :").indexOf("  module ShellTop :"))
    val root = lower(rootSource)
    val result = NativeCheckpointShellStateAccess.generate(root, ModuleTarget("Shell", "Shell"))
    assert((parse(result.mapping) \ "selected_execution") == JString("Shell"))
    assert(module(result.state, "Child") == module(result.host, "Child"))
    assert(result.entries.map(_.stateID).toSet.intersect(generated.entries.map(_.stateID).toSet).isEmpty)
  }

  it should "reject repeated definitions, stale executions, foreign targets and colliding namespaces" in {
    val state = lower()
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(state, ModuleTarget("ShellTop", "Child")))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(state,
      ModuleTarget("ShellTop", "ShellTop").instOf("missing", "Shell")))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(state, ModuleTarget("Other", "Shell")))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(state, selected, "bad_"))
    val collided = lower(source.replace("reg cycles :", "wire checkpoint_shell_taken : UInt<1>\n    checkpoint_shell_taken <= UInt<1>(0)\n    reg cycles :"))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(collided, selected))
    val reserved = lower(source.replace("cycles", "ckpt_cycles"))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(reserved, selected))
  }

  it should "preserve full host annotation owners and memory initialization while rejecting unknown or stale targets" in {
    val state = lower()
    val target = ModuleTarget("ShellTop", "Shell").ref("mem")
    val annotation = firrtl.annotations.MemoryFileInlineAnnotation(target, "shell-memory.hex")
    val keep = firrtl.transforms.DontTouchAnnotation(ModuleTarget("ShellTop", "Child").ref("count"))
    val result = NativeCheckpointShellStateAccess.generate(state.copy(annotations = state.annotations ++ Seq(annotation, keep)), selected)
    assert(result.state.annotations.contains(annotation) && result.state.annotations.contains(keep))
    assert(!result.reference.annotations.contains(annotation))
    NativeCheckpointShellStateAccess.validate(result)
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(
      state.copy(annotations = state.annotations :+ UnknownShellOwnerAnnotation()), selected))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(
      state.copy(annotations = state.annotations :+ firrtl.transforms.DontTouchAnnotation(
        ModuleTarget("ShellTop", "Child").ref("missing"))), selected))
  }

  it should "retain nonlocal initialization and preservation owners of shared children without broadening their scope" in {
    val sharedSource = source.replace("live <= count", """mem shared :
      data-type => UInt<8>
      depth => 2
      read-latency => 0
      write-latency => 1
      reader => r
      writer => w
      read-under-write => old
    shared.r.clk <= clock
    shared.r.en <= UInt<1>(1)
    shared.r.addr <= UInt<1>(0)
    shared.w.clk <= clock
    shared.w.en <= UInt<1>(0)
    shared.w.addr <= UInt<1>(0)
    shared.w.data <= count
    shared.w.mask <= UInt<1>(1)
    live <= xor(count, shared.r.data)""")
    val sourceState = lower(sharedSource)
    val left = ModuleTarget("ShellTop", "ShellTop").instOf("enclosing", "Shell").instOf("child", "Child")
    val right = ModuleTarget("ShellTop", "ShellTop").instOf("sibling", "Child")
    val init = firrtl.annotations.MemoryFileInlineAnnotation(left.ref("shared"), "left.hex")
    val keep = firrtl.transforms.DontTouchAnnotation(left.ref("count"))
    val model = midas.targetutils.FirrtlMemModelAnnotation(left.ref("shared"))
    val host = sourceState.copy(annotations = sourceState.annotations ++ Seq(init, keep, model))
    val result = NativeCheckpointShellStateAccess.generate(host, selected)
    val expected = host.annotations.filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath])
    assert(result.state.annotations.toVector == expected.toVector)
    assert(JsonProtocol.serialize(result.state.annotations) == JsonProtocol.serialize(expected))
    assert(module(result.state, "Child") == module(host, "Child"))
    assert(result.state.annotations.collect { case a: firrtl.annotations.MemoryFileInlineAnnotation => a } == Seq(init))
    assert(result.state.annotations.collect { case a: midas.targetutils.FirrtlMemModelAnnotation => a } == Seq(model))
    assert((parse(result.binding) \ "annotation_policy" \ "translation_results_applied") == JBool(false))
    assert((parse(result.binding) \ "annotation_validation" \ "validation_only") == JBool(true))
    NativeCheckpointShellStateAccess.validate(result)
    // Distinct nonlocal initializations of one shared definition must remain
    // independent. An actually overlapping local/nonlocal pair still rejects.
    val other = firrtl.annotations.MemoryFileInlineAnnotation(right.ref("shared"), "right.hex")
    val both = NativeCheckpointShellStateAccess.generate(host.copy(annotations = host.annotations :+ other), selected)
    assert(both.state.annotations.collect { case a: firrtl.annotations.MemoryFileInlineAnnotation => a } == Seq(init, other))
    val broad = firrtl.annotations.MemoryFileInlineAnnotation(ModuleTarget("ShellTop", "Child").ref("shared"), "all.hex")
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(host.copy(annotations = host.annotations :+ broad), selected))
  }

  it should "reject child-produced functional clocks and analog or aggregate child boundaries" in {
    val childClock = source.replace("output live : UInt<8>", "output live : UInt<8>\n    output generatedClock : Clock\n    generatedClock <= clock")
      .replace("reg saved : UInt<65>, clock", "reg saved : UInt<65>, child.generatedClock")
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(lower(childClock), selected))
    val castClock = childClock.replace("reg saved : UInt<65>, child.generatedClock",
      "reg saved : UInt<65>, asClock(asUInt(child.generatedClock))")
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(lower(castClock), selected))
    val analog = lower().copy(circuit = lower().circuit.mapModule {
      case m: Module if m.name == "Child" => m.copy(ports = m.ports :+ Port(NoInfo, "analog", Input, AnalogType(IntWidth(1))))
      case m => m
    })
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.generate(analog, selected))
  }

  it should "reject caller-rehashed changes to the graft, mapping, local banks and annotation policy" in {
    val result = generated
    def rehash(state: CircuitState): String = SemanticStateInventory.canonical(parse(result.binding).transformField {
      case ("instrumented_sha256", _) => "instrumented_sha256" -> JString(SemanticStateInventory.sha256(state.circuit.serialize))
    })
    val changed = result.state.copy(circuit = result.state.circuit.mapModule {
      case m: Module if m.name == "Child" => m.copy(body = Block(Seq(m.body, EmptyStmt)))
      case m => m
    })
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.validate(result.copy(state = changed, binding = rehash(changed))))
    val alteredMapping = result.mapping.replace("ShellTop.enclosing", "ShellTop.sibling")
    val alteredBinding = SemanticStateInventory.canonical(parse(result.binding).transformField {
      case ("mapping_sha256", _) => "mapping_sha256" -> JString(SemanticStateInventory.sha256(alteredMapping))
    })
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.validate(result.copy(mapping = alteredMapping, binding = alteredBinding)))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.validate(result.copy(entries = result.entries.reverse)))
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.validate(result.copy(banks = result.banks.copy(entries = result.banks.entries.reverse))))
    val policyChanged = result.state.copy(annotations = result.state.annotations :+ firrtl.annotations.MemoryNoSynthInit)
    val rehashedPolicy = SemanticStateInventory.canonical(parse(result.binding).transformField {
      case ("instrumented_annotations_sha256", _) => "instrumented_annotations_sha256" ->
        JString(SemanticStateInventory.sha256(JsonProtocol.serialize(policyChanged.annotations)))
    })
    intercept[IllegalArgumentException](NativeCheckpointShellStateAccess.validate(result.copy(state = policyChanged, binding = rehashedPolicy)))
  }

  it should "bank the exact generated Rocket enclosing owners when an explicit source pair is supplied" in {
    sys.props.get("checkpoint.shell.rocket.source").foreach { base =>
      def read(path: String): String = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path)),
        java.nio.charset.StandardCharsets.UTF_8)
      val fir = read(base + ".fir")
      val anno = read(base + ".anno.json")
      // Native JSON omits the nested annotation type for these two consumed
      // input events. Recover only their exact stage contracts, and prove a
      // full JSON round trip; arbitrary deleted owners still fail closed.
      val annotations = parse(anno).asInstanceOf[JArray].arr.flatMap { value =>
        val decoded = if ((value \ "class") == JString("firrtl.annotations.DeletedAnnotation")) {
          val JString(file) = value \ "anno" \ "file"
          Seq((value \ "xFormName") match {
            case JString("firrtl.stage.phases.AddCircuit") => firrtl.annotations.DeletedAnnotation(
              "firrtl.stage.phases.AddCircuit", firrtl.stage.FirrtlFileAnnotation(file))
            case JString("firrtl.options.phases.GetIncludes") => firrtl.annotations.DeletedAnnotation(
              "firrtl.options.phases.GetIncludes", firrtl.options.InputAnnotationFileAnnotation(file))
            case _ => fail("unsupported deleted input event")
          })
        } else JsonProtocol.deserialize(SemanticStateInventory.canonical(JArray(List(value))))
        assert(parse(JsonProtocol.serialize(decoded)) == JArray(List(value)), "annotation round trip changed " + value)
        assert(!decoded.exists(_.isInstanceOf[firrtl.annotations.UnrecognizedAnnotation]), "unrecognized source annotation " + value)
        decoded
      }
      val parsed = CircuitState(Parser.parse(fir), LowForm, annotations)
      val working = firrtl.passes.ToWorkingIR.run(parsed.circuit)
      val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
      val flowed = firrtl.passes.ResolveFlows.run(typed)
      firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
      assert(flowed.serialize == fir)
      val host = parsed.copy(circuit = flowed)
      val results = Seq("FPGATop" -> 181, "F1Shim" -> 2).map { case (name, count) =>
        val result = NativeCheckpointShellStateAccess.generate(host, ModuleTarget(host.circuit.main, name))
        assert(result.entries.size == count)
        val originalAnnotations = host.annotations.filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath])
        assert(result.state.annotations.toVector == originalAnnotations.toVector)
        assert(JsonProtocol.serialize(result.state.annotations) == JsonProtocol.serialize(originalAnnotations))
        val sourceModels = host.annotations.collect { case a: midas.targetutils.FirrtlMemModelAnnotation => a }
        val graftedModels = result.state.annotations.collect { case a: midas.targetutils.FirrtlMemModelAnnotation => a }
        assert(sourceModels.size == 4 && sourceModels == graftedModels)
        val mappings = (parse(result.mapping) \ "state").asInstanceOf[JArray].arr
        assert(mappings.forall(r => (r \ "kind") == JString("register")))
        sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { root =>
          val dir = java.nio.file.Paths.get(root, "shell-rocket", name)
          java.nio.file.Files.createDirectories(dir)
          def save(file: String, text: String): Unit = { java.nio.file.Files.write(dir.resolve(file),
            text.getBytes(java.nio.charset.StandardCharsets.UTF_8)); () }
          save("reference.fir", result.reference.circuit.serialize)
          save("generated.fir", result.state.circuit.serialize)
          save("generated.anno.json", JsonProtocol.serialize(result.state.annotations))
          save("manifest.json", result.manifest)
          save("mapping.json", result.mapping)
          save("binding.json", result.binding)
          save("banks-binding.json", result.banks.binding)
          save("source-binding.json", SemanticStateInventory.canonical(JObject(List(
            "source_base" -> JString(base), "source_fir_sha256" -> JString(SemanticStateInventory.sha256(fir)),
            "source_annotations_sha256" -> JString(SemanticStateInventory.sha256(anno)),
            "source_scope" -> JString("explicit_retained_compiler_source_new_shell_instrumentation"),
            "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false)))))
        }
        result
      }
      assert(results.flatMap(_.entries).map(_.stateID).distinct.size == 183)
    }
  }
}
