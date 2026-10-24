// See LICENSE for license details.
package midas.passes

import scala.collection.mutable
import firrtl.{CircuitState, LowForm, WDefInstance}
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.stage.RunFirrtlTransformAnnotation
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Historical result, not a live transform or a target in the bank hierarchy. */
case class SimulationInlineResultAnnotation(body: String) extends NoTargetAnnotation

private case class SimulationInlineStorage(target: ReferenceTarget, original: String)
    extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget): SimulationInlineStorage = copy(target = n)
}

private class RecordedSimulationInline extends InlineInstances {
  var renameHistory: String = ""
  override def execute(state: CircuitState): CircuitState = {
    val result = super.execute(state)
    // runTransform applies and clears this map. Record it at its producer,
    // while still letting runTransform update every real annotation normally.
    renameHistory = result.renames.map(_.serialize).getOrElse("")
    result
  }
}

/** SimulationMapping introduces wrapper InlineInstances requests after the
  * earlier target inline pass. Execute those requests on the final lowered
  * simulator BEFORE inventory/bank specialization, using FIRRTL's real rename
  * machinery. A post-bank inline pass would invalidate its hierarchy binding.
  */
object ResolveSimulationInline {
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Simulation inline unsupported: $why")

  private def statements(s: Statement): Seq[Statement] = {
    val out = mutable.ArrayBuffer.empty[Statement]
    def visit(s: Statement): Unit = { out += s; s.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  /** Every concrete register and memory instance, including unselected
    * subtrees. Opaque primitive owners remain the external-owner audit's job.
    */
  private def storage(state: CircuitState): Map[String, (ReferenceTarget, String)] = {
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    if (modules.size != state.circuit.modules.size) fail("duplicate module definitions")
    val out = mutable.LinkedHashMap.empty[String, (ReferenceTarget, String)]
    def walk(name: String, path: Seq[(TargetToken.Instance, TargetToken.OfModule)], seen: Set[String]): Unit = {
      if (seen(name)) fail("recursive hierarchy")
      modules.getOrElse(name, fail(s"missing module $name")) match {
        case m: Module => statements(m.body).foreach {
          case i: WDefInstance => walk(i.module,
            path :+ (TargetToken.Instance(i.name) -> TargetToken.OfModule(i.module)), seen + name)
          case d: DefRegister =>
            val t = ReferenceTarget(state.circuit.main, state.circuit.main, path, d.name, Nil)
            out(t.serialize) = t -> s"register:${d.tpe.serialize}"
          case d: DefMemory =>
            val t = ReferenceTarget(state.circuit.main, state.circuit.main, path, d.name, Nil)
            out(t.serialize) = t -> s"memory:${d.dataType.serialize}:${d.depth}:${d.readLatency}:${d.writeLatency}:${d.readUnderWrite}"
          case _ => ()
        }
        case _: ExtModule => ()
        case _ => fail(s"unsupported module $name")
      }
    }
    walk(state.circuit.main, Nil, Set.empty)
    out.toMap
  }

  def context(a: SimulationInlineResultAnnotation): JValue = {
    val doc = parse(a.body)
    def hash(k: String): Boolean = (doc \ k) match { case JString(s) => s.matches("[0-9a-f]{64}"); case _ => false }
    val keys = Set("version", "producer", "circuit", "request", "inline_annotations", "input_sha256",
      "output_sha256", "input_annotations_sha256", "output_annotations_sha256", "renames", "storage")
    val fields = doc match { case JObject(xs) => xs.map(_._1); case _ => fail("invalid receipt") }
    if (fields.toSet != keys || fields.distinct.size != fields.size || canonical(doc) != a.body ||
        (doc \ "version") != JInt(1) || (doc \ "producer") != JString("midas.passes.ResolveSimulationInline") ||
        !Seq("input_sha256", "output_sha256", "input_annotations_sha256", "output_annotations_sha256").forall(hash) ||
        (doc \ "request") != parse(JsonProtocol.serialize(Seq(RunFirrtlTransformAnnotation(new InlineInstances)))))
      fail("invalid consumed request receipt")
    val circuit = (doc \ "circuit") match {
      case JString(s) if s.matches("[A-Za-z_][A-Za-z0-9_$]*") => s
      case _ => fail("invalid receipt circuit")
    }
    val inline = (doc \ "inline_annotations") match {
      case a: JArray if a.arr.nonEmpty => a
      case _ => fail("missing consumed inline targets")
    }
    val targets = JsonProtocol.deserialize(canonical(inline))
    if (!targets.forall {
      case InlineAnnotation(c: CircuitName) => c.name == circuit
      case InlineAnnotation(m: ModuleName) => m.circuit.name == circuit
      case InlineAnnotation(c: ComponentName) => c.module.circuit.name == circuit
      case _ => false
    } || targets.distinct.size != targets.size)
      fail("invalid consumed inline targets")
    (doc \ "renames") match {
      case JString(s) if s.nonEmpty => ()
      case _ => fail("invalid rename history")
    }
    val rows = (doc \ "storage") match { case JArray(xs) => xs; case _ => fail("invalid storage history") }
    val origins = rows.map(_ \ "from")
    val destinations = rows.map(_ \ "to")
    if (origins.distinct.size != rows.size || destinations.distinct.size != rows.size) fail("aliased storage history")
    rows.foreach { r =>
      Seq("from", "to").foreach { key => (r \ key) match {
        case JString(s) => Target.deserialize(s) match {
          case t: ReferenceTarget if t.circuit == circuit && t.module == circuit && t.component.isEmpty => ()
          case _ => fail("invalid storage target history")
        }
        case _ => fail("missing storage target history")
      }}
      (r \ "shape") match {
        case JString(s) if s.startsWith("register:") || s.startsWith("memory:") => ()
        case _ => fail("invalid storage shape history")
      }
    }
    doc
  }

  def apply(state: CircuitState): CircuitState = {
    if (state.annotations.exists(_.isInstanceOf[SimulationInlineStorage])) fail("unexpected storage tracking input")
    val requests = state.annotations.collect {
      case a: RunFirrtlTransformAnnotation if a.transform.getClass == classOf[InlineInstances] => a
    }
    if (requests.isEmpty) return state
    if (requests.size != 1 || state.annotations.exists(_.isInstanceOf[SimulationInlineResultAnnotation]))
      fail("duplicate or repeated inline request")
    if (state.form != LowForm) fail("inline consumption requires final lowered simulator")
    val targets = state.annotations.collect { case a: InlineAnnotation => a }
    if (targets.isEmpty || targets.distinct.size != targets.size) fail("missing or duplicate inline targets")
    val before = storage(state)
    val input = state.copy(annotations = state.annotations.filterNot(_ == requests.head) ++
      before.toSeq.sortBy(_._1).map { case (id, (t, _)) => SimulationInlineStorage(t, id) })
    val pass = new RecordedSimulationInline
    val result = pass.runTransform(input)
    if (result.annotations.exists(_.isInstanceOf[InlineAnnotation])) fail("inline targets remain unconsumed")
    val tracked = result.annotations.collect { case a: SimulationInlineStorage => a }
    if (tracked.map(_.original).sorted != before.keys.toSeq.sorted) fail("lost or replicated storage during inline")
    val after = storage(result)
    val rows = tracked.sortBy(_.original).map { a =>
      if (!after.get(a.target.serialize).exists(_._2 == before(a.original)._2))
        fail(s"missing or reshaped inline storage: ${a.original} -> ${a.target.serialize}")
      JObject(List("from" -> JString(a.original), "to" -> JString(a.target.serialize),
        "shape" -> JString(before(a.original)._2)))
    }
    if (tracked.map(_.target.serialize).distinct.sorted != after.keys.toSeq.sorted)
      fail("incomplete or aliased output storage coverage")
    val clean = result.copy(annotations = result.annotations.filterNot(_.isInstanceOf[SimulationInlineStorage]))
    val receipt = SimulationInlineResultAnnotation(canonical(JObject(List(
      "version" -> JInt(1), "producer" -> JString("midas.passes.ResolveSimulationInline"),
      "circuit" -> JString(state.circuit.main), "request" -> parse(JsonProtocol.serialize(requests)),
      "inline_annotations" -> parse(JsonProtocol.serialize(targets)),
      "input_sha256" -> JString(sha256(state.circuit.serialize)),
      "output_sha256" -> JString(sha256(clean.circuit.serialize)),
      "input_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(state.annotations))),
      "output_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(clean.annotations))),
      "renames" -> JString(pass.renameHistory),
      "storage" -> JArray(rows.toList)))))
    context(receipt)
    clean.copy(annotations = clean.annotations :+ receipt)
  }
}
