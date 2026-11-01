// See LICENSE for license details.
package midas.checkpoint

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.annotations.{JsonProtocol, ModuleTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class NativeCheckpointLiveShellSnapshotSpec extends AnyFlatSpec {
  private val fields = Seq("a" -> "UInt<1>", "b" -> "UInt<64>", "c" -> "UInt<65>",
    "d" -> "SInt<9>", "e" -> "UInt<1>", "f" -> "UInt<32>", "g" -> "UInt<64>", "h" -> "UInt<1>")
  private val liveNames = fields.map(_._1).toSet
  private val source = """circuit LiveTop :
  module Shell :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<64>
    output live : UInt<237>
    reg count : UInt<64>, clock with :
      reset => (reset, UInt<64>(0))
    count <= add(count, UInt<64>(1))
    value <= count
""" + fields.map { case (name,tpe) =>
    val kind = if (tpe.startsWith("SInt")) "SInt" else "UInt"
    val initial = if (name == "c") "UInt<65>(18446744073709551619)" else s"$kind(0)"
    s"    reg $name : $tpe, clock with :\n      reset => (reset, $initial)\n" +
      s"    $name <= add($name, $kind(1))\n"
  }.mkString + """    live <= cat(h, cat(g, cat(f, cat(e, cat(asUInt(d), cat(c, cat(b, a)))))))
  module LiveTop :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<64>
    output live : UInt<237>
    inst shell of Shell
    shell.clock <= clock
    shell.reset <= reset
    value <= shell.value
    live <= shell.live
"""
  private def lower(text: String = source): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private val target = ModuleTarget("LiveTop", "LiveTop").instOf("shell", "Shell")
  private def shell(s: CircuitState = lower()) = NativeCheckpointShellStateAccess.generate(s, target,
    liveRegisters = liveNames)
  private def emit(s: CircuitState): String = {
    val emitter = new SemanticCleanEmitter
    new Compiler(Seq(firrtl.options.Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter))
      .execute(s.copy(annotations = Seq(firrtl.EmitCircuitAnnotation(emitter.getClass)))).getEmittedCircuit.value
  }
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def originalLive(s: CircuitState): Seq[Statement] = statements(s.circuit.modules.find(_.name == "Shell")
    .get.asInstanceOf[Module].body).filter {
    case r: DefRegister => liveNames(r.name)
    case c: Connect => liveNames(c.loc.serialize)
    case _ => false
  }
  behavior.of("Atomic original live shell register staging")
  it should "stage all eight original owners while preserving their equations and tying all reads to the raw clock" in {
    val s = shell(); val result = NativeCheckpointLiveShellSnapshot.generate(s)
    NativeCheckpointLiveShellSnapshot.validate(result)
    assert(originalLive(s.state) == originalLive(result.state))
    val doc = parse(result.binding)
    assert((doc \ "bits") == JInt(237) && (doc \ "words") == JInt(4))
    assert((doc \ "staging_owner_bits") == JInt(308))
    val rows = (doc \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 8 && rows.map(_ \ "state_id").distinct.size == 8)
    assert((rows(3) \ "signed") == JBool(true))
    Seq("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported", "staging_restore_supported", "target_time_authority_proven")
      .foreach(k => assert((doc \ k) == JBool(false)))
    assert(!result.state.circuit.serialize.contains("checkpoint_shell_ckpt_clock"))
    val body = result.state.circuit.modules.find(_.name == "Shell").get.asInstanceOf[Module].body.serialize
    assert(body.contains("checkpoint_shell_live_snapshot_adapter.clock <= clock"))
    assert(body.contains("checkpoint_shell_live_snapshot_adapter.borrow_attempt <= checkpoint_shell_ckpt_req_valid"))
    assert(body.contains("checkpoint_shell_live_snapshot_adapter.reader_idle <= not(checkpoint_shell_ckpt_pending)"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { base =>
      val out = java.nio.file.Paths.get(base, "live-shell-snapshot"); java.nio.file.Files.createDirectories(out)
      Seq("source.fir" -> s.host.circuit.serialize, "source.sv" -> emit(s.host),
        "banked.fir" -> s.state.circuit.serialize, "banked-binding.json" -> s.binding,
        "banked-mapping.json" -> s.mapping, "banks-binding.json" -> s.banks.binding,
        "wired.fir" -> result.state.circuit.serialize, "wired.sv" -> emit(result.state),
        "wired.anno.json" -> JsonProtocol.serialize(result.state.annotations), "binding.json" -> result.binding)
        .foreach { case (name,text) => java.nio.file.Files.write(out.resolve(name),
          text.getBytes(java.nio.charset.StandardCharsets.UTF_8)) }
    }
  }
  it should "reject missing partitions different live clocks and stale coherently rehashed generated results" in {
    intercept[IllegalArgumentException] { NativeCheckpointLiveShellSnapshot.generate(
      NativeCheckpointShellStateAccess.generate(lower(), target)) }
    val separate = source.replace("    input reset : UInt<1>", "    input otherClock : Clock\n    input reset : UInt<1>")
      .replace("reg a : UInt<1>, clock", "reg a : UInt<1>, otherClock")
      .replace("    shell.clock <= clock", "    shell.clock <= clock\n    shell.otherClock <= otherClock")
    val error = intercept[IllegalArgumentException] { NativeCheckpointLiveShellSnapshot.generate(shell(lower(separate))) }
    assert(error.getMessage.contains("one original raw input clock"))
    val gated = lower(source.replace("shell.clock <= clock",
      "shell.clock <= asClock(and(asUInt(clock), not(reset)))"))
    assert(intercept[IllegalArgumentException] {
      NativeCheckpointLiveShellSnapshot.generate(shell(gated))
    }.getMessage.contains("nontransparent ancestor raw clock"))
    val result = NativeCheckpointLiveShellSnapshot.generate(shell())
    intercept[IllegalArgumentException] { NativeCheckpointLiveShellSnapshot.validate(result.copy(
      binding = result.binding.replace("\"staging_restore_supported\":false", "\"staging_restore_supported\":true"))) }
    val changed = result.state.copy(circuit = result.state.circuit.copy(info = FileInfo(StringLit("changed"))))
    val rehashed = SemanticStateInventory.canonical(parse(result.binding) merge JObject(List(
      "wired_sha256" -> JString(SemanticStateInventory.sha256(changed.circuit.serialize)))))
    intercept[IllegalArgumentException] { NativeCheckpointLiveShellSnapshot.validate(result.copy(state = changed, binding = rehashed)) }
  }
}
