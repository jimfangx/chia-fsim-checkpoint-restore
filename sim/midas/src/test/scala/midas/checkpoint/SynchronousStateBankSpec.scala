// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.widgets.CheckpointCaptureDescriptors
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class SynchronousStateBankSpec extends AnyFlatSpec {
  private val modes = Seq(("old", false), ("new", false), ("undefined", false),
    ("old", true), ("undefined", true))
  private val memoryNames = Seq("oldMem", "newMem", "undefMem", "rwOld", "rwUndef")
  private val source = "circuit SyncStorage :\n  module SyncStorage :\n" +
    "    input clock : Clock\n    input addr : UInt<3>\n    input data : UInt<9>\n" +
    "    input ren : UInt<1>\n    input wen : UInt<1>\n    input wmode : UInt<1>\n    input mask : UInt<1>\n" +
    memoryNames.map(name => s"    output ${name}Out : UInt<9>\n").mkString +
    modes.zip(memoryNames).map { case ((ruw, rw), name) =>
      s"    mem $name :\n      data-type => UInt<9>\n      depth => 8\n" +
      (if (rw) "      readwriter => p\n" else "      reader => r\n      writer => w\n") +
      s"      read-latency => 1\n      write-latency => 1\n      read-under-write => $ruw\n" +
      (if (rw) s"    $name.p.clk <= clock\n    $name.p.en <= ren\n    $name.p.addr <= addr\n" +
        s"    $name.p.wmode <= wmode\n    $name.p.wmask <= mask\n    $name.p.wdata <= data\n" +
        s"    ${name}Out <= $name.p.rdata\n"
       else s"    $name.r.clk <= clock\n    $name.r.en <= ren\n    $name.r.addr <= addr\n" +
         s"    $name.w.clk <= clock\n    $name.w.en <= wen\n    $name.w.addr <= addr\n" +
         s"    $name.w.mask <= mask\n    $name.w.data <= data\n    ${name}Out <= $name.r.data\n")
    }.mkString
  private def lower(text: String) = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = {
    val result = new Compiler(Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
      .execute(state.copy(annotations = (state.annotations ++ Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter]))).distinct))
    result.annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
  }
  behavior.of("Synchronous storage checkpoint banks")

  it should "materialize every emitter pipeline without changing either inventory namespace" in {
    val state = lower(source)
    val semantic = SemanticStateInventory.discover(state, state.circuit.main)
    val simulator = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, semantic)
    val simResult = GeneratedStateBankAccess.generate(state, simulator)
    assert(result.entries.size == 10 && simResult.entries.size == 10)
    assert(result.entries.map(_.stateID).toSet.intersect(simResult.entries.map(_.stateID).toSet).isEmpty)
    val expected = memoryNames.map("SyncStorage." + _).toSet ++ Set(
      "SyncStorage.oldMem_r_data", "SyncStorage.newMem_r_addr", "SyncStorage.undefMem_r_addr",
      "SyncStorage.rwOld_p_rdata", "SyncStorage.rwUndef_p_rdata")
    val rows = (parse(result.binding) \ "state").asInstanceOf[JArray].arr
    assert(rows.map(e => (e \ "hierarchy").values.toString).toSet == expected)
    assert(rows.map(e => (e \ "storage_hierarchy").values).distinct.size == 10)
    assert(CheckpointCaptureDescriptors.fromGenerated(result, semantic).entries == result.entries)
    assert(CheckpointCaptureDescriptors.fromGenerated(simResult, simulator).entries == simResult.entries)
    assert(!result.state.circuit.serialize.contains("read-latency => 1"))
    assert(!result.state.circuit.serialize.contains("readwriter =>"))
    val rtl = emit(result.state)
    val reference = emit(state).replace("module SyncStorage(", "module ReferenceStorage(")
    assert(reference.contains("module ReferenceStorage("))
    // This is the original passthrough layout inventoried above, as used by
    // semantic clean emission. The generic memory-delay pass has other owners.
    assert(reference.contains("reg [8:0] rwOld_p_rdata;") &&
      reference.contains("reg [8:0] rwUndef_p_rdata;"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("synchronous"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> rtl, "reference.sv" -> reference,
        "generated-bank-semantic.json" -> semantic, "generated-bank-binding.json" -> result.binding,
        "generated-bank-simulator.json" -> simulator, "generated-bank-simulator-binding.json" -> simResult.binding,
        "generated-bank-simulator.fir" -> simResult.state.circuit.serialize
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }

  it should "reject pipeline name collisions and unsupported RW new behavior" in {
    val collision = source.replace("    mem oldMem :",
      "    reg CheckpointPipeline_oldMem_r_data : UInt<9>, clock\n    CheckpointPipeline_oldMem_r_data <= data\n    mem oldMem :")
    val state = lower(collision)
    assert(intercept[IllegalArgumentException] { GeneratedStateBankAccess.generate(state,
      SemanticStateInventory.discover(state, state.circuit.main)) }.getMessage.contains("identifier collision"))
    val unsupported = lower(source.replace("read-under-write => old", "read-under-write => new"))
    intercept[IllegalArgumentException] { GeneratedStateBankAccess.generate(unsupported,
      SimulatorStateInventory.discover(unsupported)) }
  }

  it should "resolve default read-first before inventory and retain its exact bank and emitter policy" in {
    val policies = Seq(firrtl.passes.memlib.InferReadWriteAnnotation,
      firrtl.passes.memlib.DefaultReadFirstAnnotation,
      firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation)
    val unresolved = lower(source).copy(annotations = policies)
    val e = intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(unresolved, SimulatorStateInventory.discover(unresolved))
    }
    assert(e.getMessage.contains("must be applied before inventory"))
    // The same production backend pass must select the pipeline before IDs and
    // selectors are assigned. Undefined readers now store data, not addresses.
    val state = new firrtl.passes.memlib.SetDefaultReadUnderWrite().execute(unresolved)
    val semantic = SemanticStateInventory.discover(state, state.circuit.main)
    val simulator = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, semantic)
    val simResult = GeneratedStateBankAccess.generate(state, simulator)
    policies.foreach(p => assert(result.state.annotations.contains(p)))
    CheckpointCaptureDescriptors.fromGenerated(result, semantic)
    CheckpointCaptureDescriptors.fromGenerated(simResult, simulator)
    val rows = (parse(result.binding) \ "state").asInstanceOf[JArray].arr
    assert(rows.size == 10)
    assert(rows.exists(r => (r \ "hierarchy") == JString("SyncStorage.undefMem_r_data") &&
      (r \ "width") == JInt(9)))
    assert(!rows.exists(r => (r \ "hierarchy") == JString("SyncStorage.undefMem_r_addr")))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve("synchronous-policy"); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state),
        "reference.sv" -> emit(state).replace("module SyncStorage(", "module ReferenceStorage("),
        "generated-bank-semantic.json" -> semantic, "generated-bank-binding.json" -> result.binding,
        "generated-bank-simulator.json" -> simulator, "generated-bank-simulator-binding.json" -> simResult.binding,
        "generated-bank-simulator.fir" -> simResult.state.circuit.serialize
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
}
