// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, ChirrtlForm, LowForm, Parser, EmitCircuitAnnotation}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.transforms.DontTouchAnnotation
import firrtl.passes.{InlineInstances, InlineAnnotation}
import firrtl.annotations.{CircuitName, ModuleName}
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

class SemanticFAMEStateMapSpec extends AnyFlatSpec {
  private val source = """circuit Top :
  module Leaf :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    reg time : UInt<8>, clock
    reg hidden : UInt<8>, clock
    time <= data
    hidden <= UInt<8>(3)
    value <= time
  module Top :
    input clock : Clock
    input data : UInt<8>
    output value : UInt<8>
    inst small of Leaf
    inst other of Leaf
    small.clock <= clock
    other.clock <= clock
    small.data <= data
    other.data <= add(data, UInt<8>(1))
    value <= add(small.value, other.value)
"""
  private def lower(): CircuitState = new Compiler(Forms.LowForm).execute(
    CircuitState(Parser.parse(source), ChirrtlForm)).copy(form = LowForm)
  private def seed(): CircuitState = {
    val state = lower()
    new BridgeSemanticStateInventory().execute(state.copy(annotations = state.annotations ++ Seq(
      SemanticBridgeCheckpointRootAnnotation("Top"), SemanticMultiClockInventoryAnnotation,
      SemanticFAMEStateMapAnnotation)))
  }
  private def emit(input: CircuitState): (CircuitState, String) = {
    val prepared = SemanticFAMEStateMap.prepareEmission(input)
    val emitter = new SemanticCleanEmitter
    val result = new Compiler(Seq(Dependency.fromTransform(emitter)), Forms.LowForm, Set(emitter)).execute(
      prepared.copy(annotations = prepared.annotations ++ Seq(EmitCircuitAnnotation(emitter.getClass),
        firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation)))
    (emitter.preparedState.get, result.getEmittedCircuit.value)
  }
  private def map(state: CircuitState, rtl: String): JValue = parse(SemanticFAMEStateMap.emit(state, rtl).body)

  behavior.of("Original semantic storage through compiler renames")

  it should "keep constant-driven and repeated-instance owners through flattening and actual emitter keyword renames" in {
    val initial = seed()
    val clean = initial.annotations.collect { case a: GoldenGateOutputFileAnnotation
      if a.fileSuffix == ".semantic-state.json" => a.body }.head
    assert((parse(clean) \ "state").asInstanceOf[JArray].arr.size == 4)
    // Exercise the actual FIRRTL rename map, not an emulation of flattened names.
    val flattened = new InlineInstances().runTransform(initial.copy(annotations = initial.annotations ++
      initial.circuit.modules.filter(_.name != "Top").map(m => InlineAnnotation(ModuleName(m.name, CircuitName("Top"))))))
    val (actual, rtl) = emit(SemanticFAMEStateMap.preserve(flattened))
    val doc = map(actual, rtl)
    val paths = (doc \ "state").asInstanceOf[JArray].arr.map(e => (e \ "rtl_path").values.toString)
    assert(paths.sorted == Seq("Top.other_hidden", "Top.other_time", "Top.small_hidden", "Top.small_time"))
    assert((doc \ "capture_supported") == JBool(false))
    val (hierarchical, hierarchicalRTL) = emit(initial)
    val hierarchicalPaths = (map(hierarchical, hierarchicalRTL) \ "state").asInstanceOf[JArray].arr
      .map(e => (e \ "rtl_path").values.toString)
    assert(hierarchicalPaths.contains("Top.small_.time_"))
    assert(hierarchicalPaths.contains("Top.other.time_"))
  }

  it should "reject missing duplicate reshaped and erased owners instead of fabricating values" in {
    val seeded = seed()
    val owner = seeded.annotations.collectFirst { case a: SemanticFAMEStorageAnnotation => a }.get
    for (annotations <- Seq(seeded.annotations.filterNot(_ == owner), seeded.annotations :+ owner,
        seeded.annotations.map { case a: SemanticFAMEStorageAnnotation if a == owner =>
          a.copy(target = a.target.copy(ref = "absent")); case a => a })) {
      intercept[IllegalArgumentException] {
        val (actual, rtl) = emit(seeded.copy(annotations = annotations))
        map(actual, rtl)
      }
    }
    val (actual, rtl) = emit(seeded)
    val manifest = actual.annotations.collectFirst { case a: SemanticFAMEManifestAnnotation => a }.get
    val reshaped = actual.copy(annotations = actual.annotations.map {
      case a: SemanticFAMEManifestAnnotation => a.copy(body = a.body.replace("\"width\":8", "\"width\":9"))
      case a => a
    })
    assert(manifest.body.contains("\"width\":8"))
    intercept[IllegalArgumentException](map(reshaped, rtl))
    val erased = seeded.copy(annotations = seeded.annotations.filterNot(_.isInstanceOf[DontTouchAnnotation]))
    intercept[IllegalArgumentException] {
      val (actual, rtl) = emit(erased)
      map(actual, rtl)
    }
  }

  it should "require explicit bridge-extracted schema-3 discovery and leave unselected builds unchanged" in {
    val state = lower()
    assert(SemanticFAMEStateMap.prepareEmission(state) == state)
    assert(SemanticFAMEStateMap.preserve(state) == state)
    intercept[IllegalArgumentException](SemanticStateInventory(state.copy(annotations = Seq(SemanticFAMEStateMapAnnotation))))
    intercept[IllegalArgumentException](SemanticStateInventory(state.copy(annotations = Seq(
      SemanticCheckpointRootAnnotation("Top"), SemanticMultiClockInventoryAnnotation, SemanticFAMEStateMapAnnotation))))
  }

  it should "retain exact logical memory and keyword-renamed synchronous pipeline owners" in {
    val resource = scala.io.Source.fromResource("checkpoint/MultiClockInventory.firrtl")
    val original = try resource.mkString finally resource.close()
    val state = new Compiler(Forms.LowForm).execute(CircuitState(
      Parser.parse(original.replace("ram", "large")), ChirrtlForm)).copy(form = LowForm)
    val seeded = new BridgeSemanticStateInventory().execute(state.copy(annotations = state.annotations ++ Seq(
      SemanticBridgeCheckpointRootAnnotation("MultiClockInventory"), SemanticMultiClockInventoryAnnotation,
      SemanticFAMEStateMapAnnotation)))
    val (actual, rtl) = emit(seeded)
    val paths = (map(actual, rtl) \ "state").asInstanceOf[JArray].arr.map(e => (e \ "rtl_path").values.toString)
    assert(paths.size == 9 && paths.contains("MultiClockInventory.large_") &&
      paths.contains("MultiClockInventory.large__r_data"))
  }

  it should "bind paths and the generated manifest hash to the explicitly selected nested simulator root" in {
    val wrapped = new Compiler(Forms.LowForm).execute(midas.passes.fame.WrapTop.runTransform(seed())).copy(form = LowForm)
    val selected = wrapped.copy(annotations = wrapped.annotations :+ SimulatorCheckpointInventoryAnnotation("Top"))
    val (actual, rtl) = emit(selected)
    val doc = map(actual, rtl)
    assert((doc \ "state").asInstanceOf[JArray].arr.forall(e =>
      (e \ "rtl_path").values.toString.startsWith("Top.")))
    assert((doc \ "generated_manifest_sha256") == JString(SemanticStateInventory.sha256(
      SimulatorStateInventory.discover(actual, "Top"))))
  }
}
