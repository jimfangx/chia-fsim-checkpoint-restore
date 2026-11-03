// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, Parser}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

object NativeCheckpointRegisterSnapshotFixture {
  val text = """circuit Top :
  module Leaf :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<1>
    input data : UInt<65>
    output out : UInt<65>
    reg value : UInt<65>, clock with :
      reset => (UInt<1>(0), value)
    reg signed_value : SInt<9>, clock with :
      reset => (UInt<1>(0), signed_value)
    value <= mux(reset, UInt<65>(0), mux(enable, data, value))
    signed_value <= mux(reset, SInt<9>(0), asSInt(bits(data, 8, 0)))
    out <= value
  module Fabric :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<1>
    input data : UInt<65>
    output out : UInt<65>
    inst a of Leaf
    inst b of Leaf
    a.clock <= clock
    b.clock <= clock
    a.reset <= reset
    b.reset <= reset
    a.enable <= enable
    b.enable <= not(enable)
    a.data <= data
    b.data <= xor(data, UInt<65>(123))
    out <= xor(a.out, b.out)
  module Top :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<1>
    input data : UInt<65>
    output out : UInt<65>
    inst fabric of Fabric
    inst unrelated of Leaf
    fabric.clock <= clock
    fabric.reset <= reset
    fabric.enable <= enable
    fabric.data <= data
    unrelated.clock <= clock
    unrelated.reset <= reset
    unrelated.enable <= enable
    unrelated.data <= data
    out <= xor(fabric.out, unrelated.out)
"""
  def source(value: String = text): CircuitState = {
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(
      firrtl.passes.ToWorkingIR.run(Parser.parse(value))))
    CircuitState(firrtl.passes.ResolveFlows.run(typed), LowForm)
  }
  def save(result: NativeCheckpointRegisterSnapshot.Result, name: String): Unit = {
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve(name)
    Files.createDirectories(out)
    def write(n: String, s: String): Unit = Files.write(out.resolve(n), s.getBytes(UTF_8))
    def emit(state: CircuitState): String = {
      val emitter = new SemanticCleanEmitter
      new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
        .execute(state.copy(annotations = state.annotations :+ firrtl.EmitCircuitAnnotation(emitter.getClass)))
        .getEmittedCircuit.value
    }
    write("host.fir", result.source.circuit.serialize); write("instrumented.fir", result.state.circuit.serialize)
    write("host-annotations.json", firrtl.annotations.JsonProtocol.serialize(result.source.annotations))
    write("instrumented-annotations.json", firrtl.annotations.JsonProtocol.serialize(result.state.annotations))
    write("host.sv", emit(result.source)); write("instrumented.sv", emit(result.state))
    write("binding.json", result.binding)
    result.files.foreach(f => write("generated" + f.fileSuffix, f.body))
  }
}

class NativeCheckpointRegisterSnapshotSpec extends AnyFlatSpec {
  import NativeCheckpointRegisterSnapshotFixture._
  behavior.of("Concrete live register subtree staging")
  it should "require an explicit companion regional probe and one concrete register selection" in {
    Seq(Seq(NativeCheckpointRegisterSnapshotProbeAnnotation("Top.fabric")),
      Seq(NativeCheckpointRegionCompilerProbeAnnotation("Top.fabric"),
        NativeCheckpointRegisterSnapshotProbeAnnotation("Top.fabric,Top.unrelated")),
      Seq(NativeCheckpointRegionCompilerProbeAnnotation("Top.fabric"),
        NativeCheckpointRegisterSnapshotProbeAnnotation("Top.fabric"),
        NativeCheckpointRegisterSnapshotProbeAnnotation("Top.unrelated")))
      .foreach(a => intercept[IllegalArgumentException](NativeCheckpointRegionCompilerProbe.validateCardinality(a)))
    NativeCheckpointRegionCompilerProbe.validateCardinality(Seq(
      NativeCheckpointRegionCompilerProbeAnnotation("Top.fabric"),
      NativeCheckpointRegisterSnapshotProbeAnnotation("Top.fabric")))
  }
  it should "route every shared execution separately without changing original storage or equations" in {
    val h = source()
    val r = NativeCheckpointRegisterSnapshot.generate(h, Seq("Top", "fabric"), "clock")
    val doc = parse(r.binding)
    assert((doc \ "bits") == JInt(148) && (doc \ "words") == JInt(3))
    val rows = (doc \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 4 && rows.map(x => x \ "offset") == Seq(0, 9, 74, 83).map(JInt(_)))
    assert(rows.forall(x => !(x \ "hierarchy").asInstanceOf[JString].s.contains("unrelated")))
    h.circuit.modules.foreach { original =>
      val changed = r.state.circuit.modules.find(_.name == original.name).get.asInstanceOf[Module]
      assert(changed.ports.take(original.ports.size) == original.ports)
      assert(changed.body.serialize.startsWith(original.asInstanceOf[Module].body.serialize))
    }
    NativeCheckpointRegisterSnapshot.validate(r)
    save(r, "register-snapshot")
  }
  it should "export an explicit whole staging-owner restore without altering original storage" in {
    val r = NativeCheckpointRegisterSnapshot.generate(source(), Seq("Top", "fabric"), "clock",
      restoreStaging = true)
    NativeCheckpointRegisterSnapshot.validate(r)
    val d = parse(r.binding)
    assert((d \ "snapshot_version") == JInt(3) && (d \ "staging_restore_supported") == JBool(true))
    assert((d \ "original_restore_supported") == JBool(false) &&
      (d \ "metasim_restore_supported") == JBool(false) && (d \ "cut_certificates_bound") == JBool(false))
    val root = r.state.circuit.modules.find(_.name == "Top").get.asInstanceOf[Module]
    assert(root.ports.find(_.name == "checkpoint_register_snapshot_restore_state").get.tpe == UIntType(IntWidth(219)))
    assert(root.ports.find(_.name == "checkpoint_register_snapshot_restore_ready").get.direction == Output)
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.validate(r.copy(restoreStaging = false)))
    save(r, "register-snapshot-restore")
  }
  it should "hold and load only the selected concrete originals with independent staging and no image admission" in {
    val r = NativeCheckpointRegisterSnapshot.generate(source(), Seq("Top", "fabric"), "clock",
      restoreOriginal = true)
    NativeCheckpointRegisterSnapshot.validate(r)
    val d = parse(r.binding)
    assert((d \ "snapshot_version") == JInt(4) && (d \ "original_restore_supported") == JBool(true))
    assert((d \ "staging_restore_supported") == JBool(false) && (d \ "original_restore_depth") == JInt(1))
    Seq("metasim_restore_supported", "complete_fpga_capture", "deployable_capture_image", "cut_certificates_bound")
      .foreach(n => assert((d \ n) == JBool(false)))
    val root = r.state.circuit.modules.find(_.name == "Top").get.asInstanceOf[Module]
    assert(root.body.serialize.contains("unrelated.checkpoint_register_snapshot_original_hold <= UInt<1>(\"h0\")"))
    assert(root.body.serialize.contains("fabric.checkpoint_register_snapshot_original_state <= checkpoint_register_snapshot_original_state"))
    val leaf = r.state.circuit.modules.find(_.name == "Leaf").get.asInstanceOf[Module]
    assert(leaf.body.serialize.contains("value <= mux(checkpoint_register_snapshot_original_hold, mux("))
    assert(leaf.body.serialize.contains("asSInt(bits(checkpoint_register_snapshot_original_state, 8, 0))"))
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.validate(r.copy(restoreOriginal = false)))
    val async = text.replace("reset => (UInt<1>(0), value)", "reset => (asAsyncReset(reset), UInt<65>(0))")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(source(async), Seq("Top", "fabric"),
      "clock", restoreOriginal = true))
    save(r, "register-original-restore")
  }
  it should "reject stale paths, clocks, collisions and coherently altered generated state" in {
    val h = source()
    Seq(Seq("Other"), Seq("Top", "missing")).foreach(p =>
      intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(h, p, "clock")))
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(h, Seq("Top", "fabric"), "other"))
    val r = NativeCheckpointRegisterSnapshot.generate(h, Seq("Top", "fabric"), "clock")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.validate(r.copy(binding = r.binding + " ")))
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.validate(r.copy(state = h)))
    val bad = text.replace("a.clock <= clock", "a.clock <= asClock(enable)")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(source(bad), Seq("Top", "fabric"), "clock"))
    val collision = text.replace("reg value :", "wire checkpoint_register_snapshot_collision : UInt<1>\n    reg value :")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(source(collision), Seq("Top", "fabric"), "clock"))
  }
  it should "reject memories, opaque descendants and missing resolved next-state drivers" in {
    val memory = text.replace("reg value :", "mem hidden :\n      data-type => UInt<8>\n      depth => 4\n      read-latency => 0\n      write-latency => 1\n    reg value :")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(source(memory), Seq("Top", "fabric"), "clock"))
    val missing = text.replace("    value <= mux(reset, UInt<65>(0), mux(enable, data, value))\n", "")
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(source(missing), Seq("Top", "fabric"), "clock"))
    val h = source(); val leaf = h.circuit.modules.find(_.name == "Leaf").get
    val opaque = h.copy(circuit = h.circuit.copy(modules = h.circuit.modules.map {
      case m if m.name == "Leaf" => ExtModule(NoInfo, "Leaf", leaf.ports, "OpaqueLeaf", Nil)
      case m => m
    }))
    intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(opaque, Seq("Top", "fabric"), "clock"))
  }
  it should "stage bounded asynchronous memory words at the same edge as sibling registers" in {
    val memory = text.replace("    reg value :", """    mem storage :
      data-type => SInt<9>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => read
      writer => write
      read-under-write => undefined
    storage.read.addr <= bits(data, 1, 0)
    storage.read.en <= UInt<1>(1)
    storage.read.clk <= clock
    storage.write.addr <= bits(data, 1, 0)
    storage.write.en <= enable
    storage.write.clk <= clock
    storage.write.data <= asSInt(bits(data, 10, 2))
    storage.write.mask <= bits(data, 2, 2)
    reg value :""")
    val original = NativeCheckpointRegisterSnapshot.generate(source(memory), Seq("Top", "fabric"), "clock",
      includeAsyncMemories = true, restoreStaging = true, restoreOriginal = true)
    NativeCheckpointRegisterSnapshot.validate(original)
    save(original, "register-memory-original-restore")
    val h = source(memory)
    val r = NativeCheckpointRegisterSnapshot.generate(h, Seq("Top", "fabric"), "clock", true)
    val doc = parse(r.binding)
    assert((doc \ "bits") == JInt(220) && (doc \ "snapshot_version") == JInt(2))
    assert((doc \ "state").asInstanceOf[JArray].arr.count(x => (x \ "kind") == JString("memory")) == 2)
    NativeCheckpointRegisterSnapshot.validate(r)
    save(r, "register-memory-snapshot")
    Seq(memory.replace("read-latency => 0", "read-latency => 1"),
      memory.replace("write-latency => 1", "write-latency => 2"),
      memory.replace("depth => 4", "depth => 1025"),
      memory.replace("writer => write", "writer => write\n      writer => write_again"),
      memory.replace("reader => read", "readwriter => rw\n      reader => read"),
      memory.replace("storage.write.clk <= clock", "storage.write.clk <= validif(reset, clock)"),
      memory.replace("storage.write.clk <= clock", "storage.write.clk <= asClock(enable)"),
      memory.replace("storage.read.clk <= clock", "storage.read.clk <= asClock(enable)"),
      memory.replace("read-under-write => undefined", "read-under-write => new"),
      memory.replace("reader => read", "reader => checkpoint_register_snapshot_bad"))
      .foreach { bad => intercept[IllegalArgumentException] {
        NativeCheckpointRegisterSnapshot.generate(source(bad), Seq("Top", "fabric"), "clock", true)
      }}
    NativeCheckpointRegisterSnapshot.validate(NativeCheckpointRegisterSnapshot.generate(
      source(memory.replace("storage.write.clk <= clock", "storage.write.clk <= validif(enable, clock)")),
      Seq("Top", "fabric"), "clock", true))
  }
  it should "stage each actual Rocket AW/AR reorder queue and reject the whole memory-bearing control fabric" in {
    val prefix = sys.props("checkpoint.shell.rocket.source")
    val host = source(new String(Files.readAllBytes(Paths.get(prefix + ".fir")), UTF_8))
    val control = Seq("F1Shim", "top", "ctrlInterconnect")
    val refusal = intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(host, control, "clock"))
    assert(refusal.getMessage.contains("memory or unresolved storage requires its own adapter"))
    val complete = NativeCheckpointRegisterSnapshot.generate(host, control, "clock", true)
    val completeDoc = parse(complete.binding)
    val completeRows = (completeDoc \ "state").asInstanceOf[JArray].arr
    assert(completeRows.size == 408 && completeRows.count(x => (x \ "kind") == JString("memory")) == 13)
    // Include every physical word, including all fifteen W-route FIFO entries.
    assert((completeDoc \ "bits") == JInt(1749))
    val controlName = GeneratedExternalOwners.instances(host, host.circuit.main).toMap.apply(control.mkString(".")).name
    val controlNeeded = GeneratedExternalOwners.instances(host, host.circuit.main).filter {
      case (p,_) => p == control.mkString(".") || p.startsWith(control.mkString(".") + ".")
    }.map(_._2.name).toSet
    val controlSource = host.copy(circuit = host.circuit.copy(main = controlName,
      modules = host.circuit.modules.filter(m => controlNeeded(m.name))))
    save(NativeCheckpointRegisterSnapshot.generate(controlSource, Seq(controlName), "clock", true),
      "register-memory-snapshot-rocket")
    save(NativeCheckpointRegisterSnapshot.generate(controlSource, Seq(controlName), "clock", true, true),
      "register-memory-snapshot-rocket-restore")
    save(NativeCheckpointRegisterSnapshot.generate(controlSource, Seq(controlName), "clock", true, true, true),
      "register-memory-original-restore-rocket")
    val path = control ++ Seq("xbar", "router", "aw_queue")
    val r = NativeCheckpointRegisterSnapshot.generate(host, path, "clock")
    val d = parse(r.binding)
    assert((d \ "bits") == JInt(704) && (d \ "words") == JInt(11))
    assert((d \ "state").asInstanceOf[JArray].arr.size == 192)
    val ar = NativeCheckpointRegisterSnapshot.generate(host, control ++ Seq("xbar", "router", "ar_queue"), "clock")
    assert((parse(ar.binding) \ "bits") == JInt(704))
    assert((parse(ar.binding) \ "state").asInstanceOf[JArray].arr.size == 192)
    val out = Paths.get(sys.env("CHECKPOINT_TEST_ARTIFACT_DIR")).resolve("register-snapshot-rocket-full")
    Files.createDirectories(out)
    Files.write(out.resolve("host.fir"), host.circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("instrumented.fir"), r.state.circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("binding.json"), r.binding.getBytes(UTF_8))
    // Native/VCS component qualification uses the exact selected definitions,
    // independently of the complete-image guard on the full compiler output.
    val live = GeneratedExternalOwners.instances(host, host.circuit.main).toMap
    val name = live(path.mkString(".")).name
    val needed = live.filter { case (p,_) => p == path.mkString(".") || p.startsWith(path.mkString(".") + ".") }
      .values.map(_.name).toSet
    val local = host.copy(circuit = host.circuit.copy(main = name, modules = host.circuit.modules.filter(m => needed(m.name))))
    val component = NativeCheckpointRegisterSnapshot.generate(local, Seq(name), "clock")
    save(component, "register-snapshot-rocket")
  }
}
