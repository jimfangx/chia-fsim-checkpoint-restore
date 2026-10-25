// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, WDefInstance}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class MultipleFunctionalClocksSpec extends AnyFlatSpec {
  private val source = """circuit MultiClockStorage :
  module DomainStorage :
    input a : Clock
    input b : Clock
    input reset_a : UInt<1>
    input reset_b : UInt<1>
    input data : UInt<17>
    input address : UInt<2>
    input wen : UInt<1>
    output result_a : UInt<9>
    output result_b : UInt<17>
    output result_ram : UInt<13>
    reg saved_a : UInt<9>, a with :
      reset => (reset_a, UInt<9>(0))
    reg saved_b : UInt<17>, b with :
      reset => (reset_b, UInt<17>(0))
    mem ram :
      data-type => UInt<13>
      depth => 4
      read-latency => 1
      write-latency => 1
      reader => r
      writer => w
      read-under-write => old
    ram.r.clk <= a
    ram.r.en <= UInt<1>(1)
    ram.r.addr <= address
    ram.w.clk <= b
    ram.w.en <= wen
    ram.w.addr <= address
    ram.w.data <= bits(data, 12, 0)
    ram.w.mask <= UInt<1>(1)
    saved_a <= bits(data, 8, 0)
    saved_b <= data
    result_a <= saved_a
    result_b <= saved_b
    result_ram <= ram.r.data
  module MultiClockStorage :
    input clock_a : Clock
    input clock_b : Clock
    input reset_a : UInt<1>
    input reset_b : UInt<1>
    input data : UInt<17>
    input address : UInt<2>
    input wen : UInt<1>
    output result : UInt<9>
    output left_a : UInt<9>
    output left_b : UInt<17>
    output left_ram : UInt<13>
    output right_a : UInt<9>
    output right_b : UInt<17>
    output right_ram : UInt<13>
    reg saved : UInt<9>, clock_a with :
      reset => (reset_a, UInt<9>(0))
    saved <= bits(data, 8, 0)
    result <= saved
    inst left of DomainStorage
    inst right of DomainStorage
    left.a <= clock_a
    left.b <= clock_b
    right.a <= clock_b
    right.b <= clock_a
    left.reset_a <= reset_a
    left.reset_b <= reset_b
    right.reset_a <= reset_b
    right.reset_b <= reset_a
    left.data <= data
    right.data <= data
    left.address <= address
    right.address <= address
    left.wen <= wen
    right.wen <= wen
    left_a <= left.result_a
    left_b <= left.result_b
    left_ram <= left.result_ram
    right_a <= right.result_a
    right_b <= right.result_b
    right_ram <= right.result_ram
"""
  private def lower(text: String) = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(text), ChirrtlForm)).copy(form = LowForm)
  private def emit(state: CircuitState): String = new Compiler(
    Seq(firrtl.options.Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm).execute(
      state.copy(annotations = state.annotations ++ Seq(
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation,
        firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))).getEmittedCircuit.value

  behavior.of("Explicit multiple direct functional clock domains")
  it should "preserve all clock owners and swapped repeated-child domains with independent transport" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    assert(domains.size == 6)
    assert(domains.exists(row => (row \ "clock_hierarchy") == JString("MultiClockStorage.right_.a") &&
      (row \ "root_clock") == JString("clock_b")), domains.toString)
    save(state, result, manifest, "multiple-clocks")
  }
  private def save(state: CircuitState, result: GeneratedStateBankAccess.Result, manifest: String, label: String): Unit = {
    val reference = state.copy(circuit = state.circuit.copy(main = "OriginalMultiClockStorage",
      modules = state.circuit.modules.map {
        case m: Module =>
          def rename(s: Statement): Statement = s match {
            case i: WDefInstance => i.copy(module = "Original" + i.module)
            case x => x.mapStmt(rename)
          }
          m.copy(name = "Original" + m.name, body = rename(m.body))
        case _ => fail("opaque reference")
      }))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val out = Paths.get(dir).resolve(label); Files.createDirectories(out)
      Seq("generated-bank-input.fir" -> state.circuit.serialize,
        "generated-bank.fir" -> result.state.circuit.serialize,
        "generated-bank.sv" -> emit(result.state), "reference.sv" -> emit(reference),
        "generated-bank-manifest.json" -> manifest, "generated-bank-binding.json" -> result.binding
      ).foreach { case (name, text) => Files.write(out.resolve(name), text.getBytes(UTF_8)) }
    }
  }
  it should "reject missing reordered changed and transport-owned functional domain bindings" in {
    val state = lower(source)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val domains = (doc \ "functional_clock_domains").asInstanceOf[JArray].arr
    for (bad <- Seq(JNothing, JArray(domains.tail), JArray(domains.reverse),
      JArray(domains.map(_.transformField { case ("root_clock", _) => "root_clock" -> JString("ckpt_clock") })))) {
      val binding = doc.transformField { case ("functional_clock_domains", _) => "functional_clock_domains" -> bad }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("functional clock domains"))
    }
  }
  it should "reject implicit transport selection derived clocks and derived memory clocks" in {
    val state = lower(source)
    intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state))
    }
    for (bad <- Seq(source.replace("right.a <= clock_b", "right.a <= asClock(bits(data, 0, 0))"),
      source.replace("reg saved_b : UInt<17>, b", "reg saved_b : UInt<17>, asClock(bits(data, 0, 0))"),
      source.replace("ram.w.clk <= b", "ram.w.clk <= asClock(bits(data, 0, 0))"))) {
      val input = lower(bad)
      intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(input, SimulatorStateInventory.discover(input), separateAccessClock = true)
      }
    }
  }
  private def forwardedSource: String = source
    .replace("  module DomainStorage :", """  module ClockPin :
    input pad : UInt<1>
    output value : UInt<1>
    value <= pad
  module ClockRoute :
    input input_a : Clock
    input input_b : Clock
    output a : Clock
    output b : Clock
    inst pin of ClockPin
    pin.pad <= bits(asUInt(input_a), 0, 0)
    a <= asClock(pin.value)
    node alias_b = input_b
    b <= alias_b
  module DomainStorage :""")
    .replace("    reg saved_a :", "    node local_a = a\n    reg saved_a :")
    .replace("saved_a : UInt<9>, a", "saved_a : UInt<9>, local_a")
    .replace("ram.r.clk <= a", "ram.r.clk <= local_a")
    .replace("    inst left of DomainStorage", """    inst route_a of ClockRoute
    inst route_b of ClockRoute
    route_a.input_a <= clock_a
    route_a.input_b <= route_b.b
    route_b.input_a <= clock_b
    route_b.input_b <= clock_a
    inst left of DomainStorage""")
    .replace("left.a <= clock_a", "left.a <= route_a.a")
    .replace("left.b <= clock_b", "left.b <= route_b.a")
    .replace("right.a <= clock_b", "right.a <= route_b.a")
    .replace("right.b <= clock_a", "right.b <= route_a.b")

  it should "resolve concrete sibling forwarding and bit-transparent clock IO without changing functional owners" in {
    val state = lower(forwardedSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    assert(domains.size == 14)
    assert(domains.exists(row => (row \ "clock_hierarchy") == JString("MultiClockStorage.route_a.b") &&
      (row \ "root_clock") == JString("clock_a")))
    // Reordering siblings must not make a valid routing dependency cyclic.
    val reordered = lower(forwardedSource.replace("    inst route_a of ClockRoute\n    inst route_b of ClockRoute",
      "    inst route_b of ClockRoute\n    inst route_a of ClockRoute"))
    val other = GeneratedStateBankAccess.generate(reordered, SimulatorStateInventory.discover(reordered), separateAccessClock = true)
    assert((parse(other.binding) \ "functional_clock_domains") == JArray(domains))
    save(state, result, manifest, "forwarded-clocks")
  }
  it should "reject derived sequential undriven and cyclic clock forwarding and altered output provenance" in {
    for (bad <- Seq(
      forwardedSource.replace("value <= pad", "value <= not(pad)"),
      forwardedSource.replace("pin.pad <= bits(asUInt(input_a), 0, 0)", "pin.pad <= UInt<1>(0)"),
      forwardedSource.replace("a <= asClock(pin.value)", "a is invalid"),
      forwardedSource.replace("value <= pad", "reg delayed : UInt<1>, asClock(pad)\n    delayed <= pad\n    value <= delayed"))) {
      val state = lower(bad)
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }.getMessage.contains("clock"))
    }
    val state = lower(forwardedSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val changed = doc.transformField { case ("functional_clock_domains", JArray(rows)) =>
      "functional_clock_domains" -> JArray(rows.map { row =>
        if ((row \ "clock_hierarchy") == JString("MultiClockStorage.route_a.b"))
          row.transformField { case ("root_clock", _) => "root_clock" -> JString("clock_b") }
        else row
      })
    }
    assert(intercept[IllegalArgumentException] {
      midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(binding = SemanticStateInventory.canonical(changed)), manifest)
    }.getMessage.contains("functional clock domains"))
    // Inject a netlist cycle after normal lowering (FIRRTL rejects textual loops).
    def cycle(s: Statement): Statement = s match {
      case c: Connect if c.loc.serialize == "route_b.input_b" =>
        c.copy(expr = firrtl.WSubField(firrtl.WRef("route_a"), "b", ClockType))
      case x => x.mapStmt(cycle)
    }
    val cyclic = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module if m.name == state.circuit.main => m.copy(body = cycle(m.body))
      case m => m
    }))
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(cyclic, SimulatorStateInventory.discover(cyclic), separateAccessClock = true)
    }.getMessage.contains("clock alias cycle"))
  }

  private def staticSource: String = forwardedSource
    .replace("  module DomainStorage :", """  module StaticClockRoute :
    input unused : Clock
    output forwarded : Clock
    inst pin of ClockPin
    pin.pad <= asUInt(unused)
    forwarded <= asClock(pin.value)
  module DomainStorage :
    input metadata : Clock
    output metadata_out : Clock""")
    .replace("    saved_a <= bits(data, 8, 0)", "    metadata_out <= metadata\n    saved_a <= bits(data, 8, 0)")
    .replace("    inst left of DomainStorage", """    inst static_zero of StaticClockRoute
    inst static_one of StaticClockRoute
    static_zero.unused <= asClock(UInt<1>(0))
    static_one.unused <= asClock(UInt<1>(1))
    inst left of DomainStorage""")
    .replace("    left.a <=", """    left.metadata <= static_zero.forwarded
    right.metadata <= static_one.forwarded
    left.a <=""")

  it should "bind repeated transparent static metadata clocks without inventing root domains" in {
    val state = lower(staticSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    val static = domains.filter(row => (row \ "constant_clock") != JNothing)
    assert(domains.size == 22 && static.size == 8)
    for ((instance, value) <- Seq("left_" -> 0, "right_" -> 1);
         pin <- Seq("metadata", "metadata_out")) {
      assert(static.contains(JObject(List("clock_hierarchy" -> JString(s"MultiClockStorage.$instance.$pin"),
        "constant_clock" -> JInt(value)))))
    }
    assert(static.forall(row => (row \ "root_clock") == JNothing && (row \ "gate_chain") == JNothing))
    save(state, result, manifest, "static-clocks")
  }

  it should "reject static clocks on every register memory pipeline and clocked effect consumer" in {
    for (replacement <- Seq(
      "reg saved_a : UInt<9>, metadata",
      "reg saved_a : UInt<9>, asClock(UInt<1>(0))",
      "reg saved_a : UInt<9>, asClock(UInt<1>(1))")) {
      val state = lower(staticSource.replace("reg saved_a : UInt<9>, local_a", replacement))
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }.getMessage.contains("constant clock drives functional"))
    }
    for (bad <- Seq(
      staticSource.replace("ram.r.clk <= local_a", "ram.r.clk <= metadata"),
      staticSource.replace("ram.w.clk <= b", "ram.w.clk <= metadata"),
      staticSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    printf(metadata, wen, \"static effect\\n\")"),
      staticSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    stop(metadata, wen, 1)"),
      staticSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    assert(metadata, wen, UInt<1>(1), \"static verify\")"))) {
      val state = lower(bad)
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }.getMessage.contains("constant clock drives functional"))
    }
  }

  it should "independently reject altered static bindings and storage clock rewiring with updated circuit hash" in {
    val state = lower(staticSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val domains = (doc \ "functional_clock_domains").asInstanceOf[JArray].arr
    val static = domains.filter(row => (row \ "constant_clock") != JNothing)
    for (bad <- Seq(JArray(domains.filterNot(static.contains)),
      JArray(domains.map(_.transformField { case ("constant_clock", _) => "constant_clock" -> JInt(2) })),
      JArray(domains.map(_.transformField { case ("constant_clock", _) => "root_clock" -> JString("clock_a") })))) {
      val binding = doc.transformField { case ("functional_clock_domains", _) => "functional_clock_domains" -> bad }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("functional clock domains"))
    }
    for (consumer <- Seq("register", "memory")) {
      def rewire(s: Statement): Statement = s match {
        case r: DefRegister if consumer == "register" && r.name == "saved_a" =>
          r.copy(clock = firrtl.WRef("metadata", ClockType))
        case c: firrtl.ir.Connect if consumer == "memory" && c.loc.serialize == "ram.w.clk" =>
          c.copy(expr = firrtl.WRef("metadata", ClockType))
        case x => x.mapStmt(rewire)
      }
      val mutated = result.state.copy(circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
        case m: Module if m.ports.exists(_.name == "metadata") => m.copy(body = rewire(m.body))
        case m => m
      }))
      val binding = doc.transformField { case ("instrumented_sha256", _) =>
        "instrumented_sha256" -> JString(SemanticStateInventory.sha256(mutated.circuit.serialize)) }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(state = mutated,
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("constant clock drives functional"))
    }
  }

  private def invalidSource: String = staticSource
    .replace("static_zero.unused <= asClock(UInt<1>(0))", "static_zero.unused is invalid")
    .replace("static_one.unused <= asClock(UInt<1>(1))", "static_one.unused is invalid")

  private def dataSource: String = staticSource
    .replace("static_zero.unused <= asClock(UInt<1>(0))", "static_zero.unused <= asClock(eq(bits(data, 0, 0), UInt<1>(0)))")
    .replace("static_one.unused <= asClock(UInt<1>(1))", "static_one.unused <= asClock(lt(data, UInt<17>(65536)))")
    .replace("metadata_out <= metadata\n    saved_a <= bits(data, 8, 0)",
      "metadata_out <= metadata\n    saved_a <= mux(asUInt(metadata), bits(data, 8, 0), UInt<9>(0))")

  it should "preserve computed Clock token data through repeated routes and functional data consumers" in {
    val state = lower(dataSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    val data = domains.filter(row => (row \ "data_clock") != JNothing)
    assert(domains.size == 22 && data.size == 8)
    for ((side, route, expression) <- Seq(("left_", "zero", "eq(bits(data, 0, 0), UInt<1>(\"h0\"))"),
      ("right_", "one", "lt(data, UInt<17>(\"h10000\"))"));
      pin <- Seq(s"$side.metadata", s"$side.metadata_out", s"static_$route.unused", s"static_$route.forwarded")) {
      assert(data.contains(JObject(List("clock_hierarchy" -> JString(s"MultiClockStorage.$pin"),
        "data_clock" -> JObject(List("hierarchy" -> JString("MultiClockStorage"), "expression" -> JString(expression)))))))
    }
    save(state, result, manifest, "data-clocks")
  }

  it should "reject computed token data as any sequential clock including memory pipelines and effects" in {
    for (bad <- Seq(
      dataSource.replace("reg saved_a : UInt<9>, local_a", "reg saved_a : UInt<9>, metadata"),
      dataSource.replace("ram.r.clk <= local_a", "ram.r.clk <= metadata"),
      dataSource.replace("ram.w.clk <= b", "ram.w.clk <= metadata"),
      dataSource.replace("saved_b <= data", "saved_b <= data\n    printf(metadata, wen, \"token effect\\n\")"),
      dataSource.replace("saved_b <= data", "saved_b <= data\n    stop(metadata, wen, 1)"),
      dataSource.replace("saved_b <= data", "saved_b <= data\n    assert(metadata, wen, UInt<1>(1), \"token verify\")"))) {
      val state = lower(bad)
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }.getMessage.contains("computed clock data drives functional"))
    }
  }

  it should "preserve one-bit root token data without admitting it as a functional clock" in {
    val tokenSource = dataSource.replace("module MultiClockStorage :\n    input clock_a : Clock",
      "module MultiClockStorage :\n    input root_token : UInt<1>\n    input clock_a : Clock")
      .replace("asClock(eq(bits(data, 0, 0), UInt<1>(0)))", "asClock(root_token)")
      .replace("asClock(lt(data, UInt<17>(65536)))", "asClock(root_token)")
    val state = lower(tokenSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    assert(domains.count(row => (row \ "data_clock" \ "expression") == JString("root_token")) == 8)
    for (bad <- Seq(
      tokenSource.replace("reg saved_a : UInt<9>, local_a", "reg saved_a : UInt<9>, metadata"),
      tokenSource.replace("ram.r.clk <= local_a", "ram.r.clk <= metadata"),
      tokenSource.replace("ram.w.clk <= b", "ram.w.clk <= metadata"),
      tokenSource.replace("saved_b <= data", "saved_b <= data\n    stop(metadata, wen, 1)"))) {
      val mutated = lower(bad)
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(mutated, SimulatorStateInventory.discover(mutated), separateAccessClock = true)
      }.getMessage.contains("computed clock data drives functional"))
    }
  }

  it should "independently reconstruct token origins and reject coherently rehashed sequential rewiring" in {
    val state = lower(dataSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val domains = (doc \ "functional_clock_domains").asInstanceOf[JArray].arr
    for (bad <- Seq(JArray(domains.filter(row => (row \ "data_clock") == JNothing)),
      JArray(domains.map(_.transformField { case ("hierarchy", _) => "hierarchy" -> JString("invented") })),
      JArray(domains.map(_.transformField { case ("expression", _) => "expression" -> JString("invented") })),
      JArray(domains.map(_.transformField { case ("data_clock", _) => "root_clock" -> JString("clock_a") })))) {
      val binding = doc.transformField { case ("functional_clock_domains", _) => "functional_clock_domains" -> bad }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("functional clock domains"))
    }
    for (consumer <- Seq("register", "memory", "effect")) {
      def rewire(s: Statement): Statement = s match {
        case r: DefRegister if consumer == "register" && r.name == "saved_a" =>
          r.copy(clock = firrtl.WRef("metadata", ClockType))
        case c: Connect if consumer == "memory" && c.loc.serialize == "ram.w.clk" =>
          c.copy(expr = firrtl.WRef("metadata", ClockType))
        case x => x.mapStmt(rewire)
      }
      val mutated = result.state.copy(circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
        case m: Module if m.ports.exists(_.name == "metadata") =>
          val body = rewire(m.body)
          m.copy(body = if (consumer != "effect") body else Block(Seq(body,
            Stop(NoInfo, 1, firrtl.WRef("metadata", ClockType), UIntLiteral(1, IntWidth(1))))))
        case m => m
      }))
      val binding = doc.transformField { case ("instrumented_sha256", _) =>
        "instrumented_sha256" -> JString(SemanticStateInventory.sha256(mutated.circuit.serialize)) }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(state = mutated,
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("computed clock data drives functional"))
    }
  }

  it should "preserve explicitly invalid metadata origins through repeated transparent hierarchy" in {
    val state = lower(invalidSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result, manifest)
    assert(result.entries.size == 9 && result.entries.map(_.depth).sum == 15)
    val domains = (parse(result.binding) \ "functional_clock_domains").asInstanceOf[JArray].arr
    val invalid = domains.filter(row => (row \ "invalid_clock") != JNothing)
    assert(domains.size == 22 && invalid.size == 8)
    for ((side, route) <- Seq("left_" -> "static_zero", "right_" -> "static_one");
         pin <- Seq(s"$side.metadata", s"$side.metadata_out", s"$route.unused", s"$route.forwarded")) {
      assert(invalid.contains(JObject(List("clock_hierarchy" -> JString(s"MultiClockStorage.$pin"),
        "invalid_clock" -> JString(s"MultiClockStorage.$route.unused")))))
    }
    assert(invalid.forall(row => (row \ "root_clock") == JNothing &&
      (row \ "constant_clock") == JNothing && (row \ "gate_chain") == JNothing))
    save(state, result, manifest, "invalid-clocks")
  }

  it should "reject invalid metadata on registers memory ports pipelines and clocked effects" in {
    for (bad <- Seq(
      invalidSource.replace("reg saved_a : UInt<9>, local_a", "reg saved_a : UInt<9>, metadata"),
      invalidSource.replace("ram.r.clk <= local_a", "ram.r.clk <= metadata"),
      invalidSource.replace("ram.w.clk <= b", "ram.w.clk <= metadata"),
      invalidSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    printf(metadata, wen, \"invalid effect\\n\")"),
      invalidSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    stop(metadata, wen, 1)"),
      invalidSource.replace("saved_a <= bits(data, 8, 0)",
        "saved_a <= bits(data, 8, 0)\n    assert(metadata, wen, UInt<1>(1), \"invalid verify\")"))) {
      val state = lower(bad)
      assert(intercept[IllegalArgumentException] {
        GeneratedStateBankAccess.generate(state, SimulatorStateInventory.discover(state), separateAccessClock = true)
      }.getMessage.contains("invalid clock drives functional"))
    }
    // An absent connection is not evidence of an intentional invalid pin.
    val state = lower(invalidSource)
    def drop(s: Statement): Statement = s match {
      case i: IsInvalid if i.expr.serialize == "static_zero.unused" => EmptyStmt
      case x => x.mapStmt(drop)
    }
    val undriven = state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module if m.name == state.circuit.main => m.copy(body = drop(m.body))
      case m => m
    }))
    assert(intercept[IllegalArgumentException] {
      GeneratedStateBankAccess.generate(undriven, SimulatorStateInventory.discover(undriven), separateAccessClock = true)
    }.getMessage.contains("missing child clock"))
  }

  it should "reconstruct invalid provenance and reject changed binding or rewired functional clocks independently" in {
    val state = lower(invalidSource)
    val manifest = SimulatorStateInventory.discover(state)
    val result = GeneratedStateBankAccess.generate(state, manifest, separateAccessClock = true)
    val doc = parse(result.binding)
    val domains = (doc \ "functional_clock_domains").asInstanceOf[JArray].arr
    for (bad <- Seq(JArray(domains.filter(row => (row \ "invalid_clock") == JNothing)),
      JArray(domains.map(_.transformField { case ("invalid_clock", _) => "invalid_clock" -> JString("invented") })),
      JArray(domains.map(_.transformField { case ("invalid_clock", _) => "root_clock" -> JString("clock_a") })),
      JArray(domains.map(_.transformField { case ("invalid_clock", _) => "constant_clock" -> JInt(0) })))) {
      val binding = doc.transformField { case ("functional_clock_domains", _) => "functional_clock_domains" -> bad }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("functional clock domains"))
    }
    for (consumer <- Seq("register", "memory", "effect")) {
      def rewire(s: Statement): Statement = s match {
        case r: DefRegister if consumer == "register" && r.name == "saved_a" =>
          r.copy(clock = firrtl.WRef("metadata", ClockType))
        case c: Connect if consumer == "memory" && c.loc.serialize == "ram.w.clk" =>
          c.copy(expr = firrtl.WRef("metadata", ClockType))
        case x => x.mapStmt(rewire)
      }
      val mutated = result.state.copy(circuit = result.state.circuit.copy(modules = result.state.circuit.modules.map {
        case m: Module if m.ports.exists(_.name == "metadata") =>
          val body = rewire(m.body)
          m.copy(body = if (consumer != "effect") body else Block(Seq(body,
            Stop(NoInfo, 1, firrtl.WRef("metadata", ClockType), UIntLiteral(1, IntWidth(1))))))
        case m => m
      }))
      val binding = doc.transformField { case ("instrumented_sha256", _) =>
        "instrumented_sha256" -> JString(SemanticStateInventory.sha256(mutated.circuit.serialize)) }
      assert(intercept[IllegalArgumentException] {
        midas.widgets.CheckpointCaptureDescriptors.fromGenerated(result.copy(state = mutated,
          binding = SemanticStateInventory.canonical(binding)), manifest)
      }.getMessage.contains("invalid clock drives functional"))
    }
  }
}
