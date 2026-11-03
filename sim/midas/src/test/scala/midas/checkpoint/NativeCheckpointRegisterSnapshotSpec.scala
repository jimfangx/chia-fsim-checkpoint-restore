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
  it should "stage each actual Rocket AW/AR reorder queue and reject the whole memory-bearing control fabric" in {
    val prefix = sys.props("checkpoint.shell.rocket.source")
    val host = source(new String(Files.readAllBytes(Paths.get(prefix + ".fir")), UTF_8))
    val control = Seq("F1Shim", "top", "ctrlInterconnect")
    val refusal = intercept[IllegalArgumentException](NativeCheckpointRegisterSnapshot.generate(host, control, "clock"))
    assert(refusal.getMessage.contains("memory or unresolved storage requires its own adapter"))
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
