// See LICENSE for license details.
package midas.passes

import firrtl.{ChirrtlForm, CircuitState, LowForm, Parser, Transform, WDefInstance}
import firrtl.annotations.{ModuleTarget, ReferenceTarget}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import midas.checkpoint.NativeCheckpointRegionCompilerProbeAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointRegionHostClockWiringSpec extends AnyFlatSpec {
  private val text = """circuit Top :
  module Consumer :
    input clock : Clock
    output value : UInt<8>
    reg count : UInt<8>, clock
    count <= add(count, UInt<8>(1))
    value <= count
  module Host :
    input clock : Clock
    input otherClock : Clock
    input data : UInt<1>
    output mirrorClock : Clock
    output value : UInt<8>
    inst consumer of Consumer
    consumer.clock <= otherClock
    mirrorClock <= clock
    value <= consumer.value
  module Top :
    input clock : Clock
    input otherClock : Clock
    output value : UInt<8>
    inst host of Host
    host.clock <= clock
    host.otherClock <= otherClock
    host.data <= UInt<1>(0)
    value <= host.value
"""
  private val clock = HostClockSource(ModuleTarget("Top", "Host").ref("clock"))
  private val sink = HostClockSink(ReferenceTarget("Top", "Host", Nil, "consumer", Nil).field("clock"))
  private val request = NativeCheckpointRegionCompilerProbeAnnotation("Top.host.consumer")
  private def input = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(text), ChirrtlForm))
    .copy(form = LowForm, annotations = Seq(clock, sink))
  private def probe = input.copy(annotations = input.annotations :+ request)
  private def sources(state: CircuitState) = state.annotations.collect { case a: HostClockSource => a }.toVector

  behavior.of("Actual host clock label retained for the regional diagnostic")

  it should "retain one real source with identical native wiring while ordinary execute still consumes it" in {
    val ordinary = HostClockWiring.execute(input)
    assert(sources(ordinary).isEmpty)
    assert(!ordinary.annotations.exists(_.isInstanceOf[HostClockSink]))
    val documented = HostClockWiring(input)
    assert(sources(documented) == Vector(clock))
    val selected = probe
    val regional = CheckpointRegionHostClockWiring.runTransform(selected)
    assert(sources(regional) == Vector(clock))
    assert(!regional.annotations.exists(_.isInstanceOf[HostClockSink]))
    assert(regional.circuit == HostClockWiring.execute(selected).circuit)
    assert(regional.circuit.serialize.contains("consumer.clock <= HostClockSource"))
    assert(regional.circuit.serialize.contains("HostClockSource <= clock"))
    // A second documented retaining invocation cannot duplicate the source.
    assert(sources(CheckpointRegionHostClockWiring.runTransform(regional)) == Vector(clock))
  }

  it should "reject absent duplicate stale foreign indexed output or non-clock sources without guessing a replacement" in {
    intercept[IllegalArgumentException](CheckpointRegionHostClockWiring.execute(input))
    val selected = probe
    val invalid = Seq(selected.annotations.filterNot(_.isInstanceOf[HostClockSource]), selected.annotations :+ clock) ++
      Seq(ModuleTarget("Other", "Host").ref("clock"), ModuleTarget("Top", "Missing").ref("clock"),
        ModuleTarget("Top", "Host").ref("missing"), ModuleTarget("Top", "Host").ref("data"),
        ModuleTarget("Top", "Host").ref("mirrorClock"), ModuleTarget("Top", "Host").ref("clock").index(0))
        .map(t => selected.annotations.filterNot(_.isInstanceOf[HostClockSource]) :+ HostClockSource(t))
    invalid.foreach(as => intercept[IllegalArgumentException] {
      CheckpointRegionHostClockWiring.execute(selected.copy(annotations = as))
    })
    intercept[IllegalArgumentException](CheckpointRegionHostClockWiring.execute(selected.copy(annotations = selected.annotations :+ request)))
  }

  it should "follow an actual native target rename and reject the old source after relocation" in {
    val rename = new Transform {
      def inputForm = LowForm
      def outputForm = LowForm
      def execute(state: CircuitState): CircuitState = {
        def statement(s: Statement): Statement = s match {
          case i: WDefInstance if i.module == "Host" => i.copy(module = "MovedHost")
          case x => x.mapStmt(statement)
        }
        val modules = state.circuit.modules.map {
          case m: Module => m.copy(name = if (m.name == "Host") "MovedHost" else m.name, body = statement(m.body))
          case m => m
        }
        val renames = firrtl.RenameMap()
        renames.record(ModuleTarget("Top", "Host"), ModuleTarget("Top", "MovedHost"))
        state.copy(circuit = state.circuit.copy(modules = modules), renames = Some(renames))
      }
    }
    val moved = rename.runTransform(probe)
    assert(sources(moved).map(_.target.module) == Vector("MovedHost"))
    val wired = CheckpointRegionHostClockWiring.runTransform(moved)
    assert(sources(wired) == sources(moved))
    val stale = moved.copy(annotations = moved.annotations.filterNot(_.isInstanceOf[HostClockSource]) :+ clock)
    intercept[IllegalArgumentException](CheckpointRegionHostClockWiring.execute(stale))
  }
}
