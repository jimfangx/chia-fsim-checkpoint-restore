// See LICENSE for license details.
package midas.passes

import firrtl.{CircuitState, HighForm, LowForm, Parser, Transform, WDefInstance}
import firrtl.annotations._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.DontTouchAnnotation
import midas.core.TargetBoxAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class SimulationMappingSpec extends AnyFlatSpec {
  private val inner = CircuitState(Parser.parse("""circuit FAMETop :
  module FAMETop :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<8>
    reg count : UInt<8>, clock with :
      reset => (reset, UInt<8>(0))
    count <= add(count, UInt<8>(1))
    value <= count
"""), LowForm, Seq(DontTouchAnnotation(ModuleTarget("FAMETop", "FAMETop").ref("count"))))
  private val outerCircuit = Parser.parse("""circuit Host :
  extmodule Placeholder :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<8>
    defname = TargetBox
  module Wrapper :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<8>
    inst target of Placeholder
    target.clock <= clock
    target.reset <= reset
    value <= target.value
  module Host :
    input clock : Clock
    input reset : UInt<1>
    output value : UInt<8>
    inst sim of Wrapper
    sim.clock <= clock
    sim.reset <= reset
    value <= sim.value
""")
  private val path = ModuleTarget("Host", "Host").instOf("sim", "Wrapper").instOf("target", "Placeholder")
  private def outer = CircuitState(outerCircuit, LowForm, Seq(TargetBoxAnnotation(path),
    DontTouchAnnotation(path.ref("value")),
    DontTouchAnnotation(ModuleTarget("Host", "Placeholder").ref("clock"))))
  // Compiler executes the returned native rename map, as in Golden Gate.
  private def nativeLink(in: CircuitState = inner, out: CircuitState = outer): CircuitState = {
    val linker = new Transform {
      def inputForm = LowForm
      def outputForm = HighForm
      def execute(s: CircuitState): CircuitState = SimulationMapping.link(s, out)
    }
    new Compiler(Seq(Dependency.fromTransform(linker)), Forms.LowForm, Set(linker)).execute(in)
  }
  private def reject(out: CircuitState, message: String, in: CircuitState = inner): Unit = {
    assert(intercept[IllegalArgumentException] { SimulationMapping.link(in, out) }
      .getMessage.contains(message))
  }

  behavior.of("Native simulation target linking")

  it should "rename the exact boundary and every local/nonlocal annotation through FIRRTL" in {
    val linked = nativeLink()
    val expected = path.copy(ofModule = "FAMETop")
    assert(linked.annotations.collect { case a: TargetBoxAnnotation => a.target } == Seq(expected))
    val refs = linked.annotations.collect { case a: DontTouchAnnotation => a.target }.toSet
    assert(refs == Set(expected.ref("value"), ModuleTarget("Host", "FAMETop").ref("clock"),
      ModuleTarget("Host", "FAMETop").ref("count")))
    assert(!linked.circuit.modules.exists(_.name == "Placeholder"))
    val wrapper = linked.circuit.modules.collectFirst { case m: Module if m.name == "Wrapper" => m }.get
    val instances = scala.collection.mutable.ArrayBuffer.empty[WDefInstance]
    def visit(s: Statement): Unit = {
      s match { case i: WDefInstance => instances += i; case _ => () }; s.foreachStmt(visit)
    }
    visit(wrapper.body)
    assert(instances.map(i => (i.name, i.module)) == Seq("target" -> "FAMETop"))
    assert(instances.head.tpe == firrtl.Utils.module_type(inner.circuit.modules.head))
    assert(JsonProtocol.deserialize(JsonProtocol.serialize(linked.annotations)).toSeq == linked.annotations.toSeq)
  }

  it should "survive native low-form checking and strict generated-bank boundary specialization" in {
    val linked = nativeLink()
    val low = new Compiler(Forms.LowForm).execute(linked).copy(form = LowForm)
    val manifest = midas.checkpoint.SemanticStateInventory.discover(low, "Host")
    val bank = midas.checkpoint.GeneratedStateBankAccess.generate(low, manifest, separateAccessClock = true)
    val targets = bank.state.annotations.collect { case a: TargetBoxAnnotation => a.target }
    assert(targets.size == 1 && targets.head.asPath.map(_._1.value) == Seq("sim", "target"))
    assert(!targets.head.serialize.contains("Placeholder"))
    sys.env.get("CHECKPOINT_TEST_ARTIFACT_DIR").foreach { dir =>
      val dest = java.nio.file.Paths.get(dir, "simulation-mapping")
      java.nio.file.Files.createDirectories(dest)
      def emit(s: CircuitState): String = new Compiler(Seq(Dependency[firrtl.SystemVerilogEmitter]), Forms.LowForm)
        .execute(s.copy(annotations = s.annotations :+ firrtl.EmitCircuitAnnotation(classOf[firrtl.SystemVerilogEmitter])))
        .annotations.collectFirst { case firrtl.EmittedVerilogCircuitAnnotation(v) => v.value }.get
      val reference = Seq("Host", "Wrapper", "FAMETop").foldLeft(emit(low)) { case (text, name) =>
        text.replaceAll("\\b" + name + "\\b", "Original" + name)
      }
      Seq("inner.fir" -> inner.circuit.serialize, "outer.fir" -> outer.circuit.serialize,
        "outer.anno.json" -> JsonProtocol.serialize(outer.annotations),
        "linked.fir" -> low.circuit.serialize,
        "linked.anno.json" -> JsonProtocol.serialize(low.annotations),
        "bank.fir" -> bank.state.circuit.serialize,
        "bank.anno.json" -> JsonProtocol.serialize(bank.state.annotations),
        "manifest.json" -> manifest, "binding.json" -> bank.binding,
        "bank.sv" -> emit(bank.state), "reference.sv" -> reference).foreach { case (name, value) =>
        java.nio.file.Files.write(dest.resolve(name), value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      }
    }
  }

  it should "use pre-link collision-renamed placeholder names rather than hardcoded TargetBox" in {
    val renamed = outer.copy(circuit = outer.circuit.copy(modules = outer.circuit.modules.map {
      case m: ExtModule => m.copy(name = "TargetBox_17")
      case m: Module => m.copy(body = PreLinkRenaming.updateInsts(
        Map("Placeholder" -> "TargetBox_17", "Wrapper" -> "Wrapper"))(m.body))
    }), annotations = Seq(TargetBoxAnnotation(path.copy(ofModule = "TargetBox_17"))))
    assert(nativeLink(out = renamed).annotations.collect { case a: TargetBoxAnnotation => a.target } ==
      Seq(path.copy(ofModule = "FAMETop")))
  }

  it should "reject missing, duplicate and module-only target boundary annotations" in {
    reject(outer.copy(annotations = Nil), "exactly one")
    reject(outer.copy(annotations = Seq.fill(2)(TargetBoxAnnotation(path))), "exactly one")
    reject(outer.copy(annotations = Seq(TargetBoxAnnotation(ModuleTarget("Host", "Placeholder")))), "must be an instance")
  }

  it should "reject foreign, relative, dead and mistyped boundary paths" in {
    Seq(path.copy(circuit = "Foreign"), ModuleTarget("Host", "Wrapper").instOf("target", "Placeholder"))
      .foreach(t => reject(outer.copy(annotations = Seq(TargetBoxAnnotation(t))), "rooted"))
    Seq(path.copy(instance = "missing"), path.copy(ofModule = "FAMETop"),
      ModuleTarget("Host", "Host").instOf("sim", "Missing").instOf("target", "Placeholder"))
      .foreach(t => reject(outer.copy(annotations = Seq(TargetBoxAnnotation(t))), "path mismatch"))
  }

  it should "reject multiple placeholder uses including repeated concrete wrapper instances" in {
    val repeated = outer.circuit.copy(modules = outer.circuit.modules.map {
      case m: Module if m.name == "Host" => m.copy(body = Block(Seq(m.body, WDefInstance("other", "Wrapper"))))
      case m => m
    })
    reject(outer.copy(circuit = repeated), "exactly one concrete instance")
    val direct = outer.circuit.copy(modules = outer.circuit.modules.map {
      case m: Module if m.name == "Host" => m.copy(body = Block(Seq(m.body, WDefInstance("other", "Placeholder"))))
      case m => m
    })
    reject(outer.copy(circuit = direct), "exactly one instance")
  }

  it should "reject recursive hierarchy and missing module definitions" in {
    Seq("Host" -> "recursive", "Missing" -> "missing outer module").foreach { case (module, error) =>
      val circuit = outer.circuit.copy(modules = outer.circuit.modules.map {
        case m: Module if m.name == "Host" => m.copy(body = Block(Seq(m.body, WDefInstance("other", module))))
        case m => m
      })
      reject(outer.copy(circuit = circuit), error)
    }
  }

  it should "reject nonexternal placeholders and incompatible target interfaces" in {
    val concrete = outer.circuit.copy(modules = outer.circuit.modules.map {
      case m: ExtModule => Module(m.info, m.name, m.ports, EmptyStmt)
      case m => m
    })
    reject(outer.copy(circuit = concrete), "placeholder must be an external")
    val changed = outer.circuit.copy(modules = outer.circuit.modules.map {
      case m: ExtModule => m.copy(ports = m.ports.map(p => if (p.name == "value") p.copy(tpe = UIntType(IntWidth(9))) else p))
      case m => m
    })
    reject(outer.copy(circuit = changed), "interfaces differ")
  }

  it should "reject colliding namespaces, duplicate definitions and an external inner top" in {
    reject(outer.copy(circuit = outer.circuit.copy(modules = outer.circuit.modules :+ inner.circuit.modules.head)), "namespaces overlap")
    reject(outer.copy(circuit = outer.circuit.copy(modules = outer.circuit.modules :+ outer.circuit.modules.head)), "duplicate module")
    val external = inner.copy(circuit = inner.circuit.copy(modules = inner.circuit.modules.map {
      case m: Module => ExtModule(m.info, m.name, m.ports, m.name, Nil)
      case m => m
    }))
    reject(outer, "inner top must be a concrete", external)
  }
}
