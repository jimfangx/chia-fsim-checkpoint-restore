// See LICENSE for license details.
package midas.passes

import firrtl.{CircuitState, AnnotationSeq}
import firrtl.annotations._
import firrtl.transforms.{DedupGroupAnnotation, NoCircuitDedupAnnotation, CheckpointGroupDedupModules}
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Immutable compiler history; these are never live hierarchy targets. */
case class DedupGroupPolicyAnnotation(body: String) extends NoTargetAnnotation
case class DedupGroupResultAnnotation(body: String) extends NoTargetAnnotation
case class DedupGroupRetirementAnnotation(body: String) extends NoTargetAnnotation

object GoldenGateDedupGroups {
  private val policy = "firrtl-1.6-group-aware-forced-dedup-v1"
  private def fail(s: String): Nothing = throw new IllegalArgumentException(s"Golden Gate dedup groups: $s")
  private def fields(doc: JValue, keys: Set[String]): Unit = doc match {
    case JObject(xs) if xs.map(_._1).toSet == keys && xs.map(_._1).distinct.size == xs.size => ()
    case _ => fail("invalid history fields")
  }
  private def hash(doc: JValue, key: String): Unit = (doc \ key) match {
    case JString(s) if s.matches("[0-9a-f]{64}") => ()
    case _ => fail(s"invalid $key")
  }
  private def name(s: String): Boolean = s.matches("[A-Za-z_][A-Za-z0-9_$]*")
  private def text(doc: JValue, key: String): String = (doc \ key) match {
    case JString(s) => s
    case _ => fail(s"invalid $key")
  }
  private def groups(state: CircuitState): Seq[DedupGroupAnnotation] = {
    val values = state.annotations.collect { case a: DedupGroupAnnotation => a }
    val modules = state.circuit.modules.map(_.name)
    if (modules.distinct.size != modules.size) fail("duplicate module definition")
    values.foreach { a =>
      if (a.target.circuit != state.circuit.main || !modules.contains(a.target.module) ||
          a.group == null || a.group.isEmpty || a.group.contains('\u0000') ||
          !java.nio.charset.StandardCharsets.UTF_8.newEncoder().canEncode(a.group))
        fail(s"invalid group target or value: ${a.target.serialize}")
    }
    if (values.map(_.target).distinct.size != values.size) fail("duplicate or conflicting module group")
    values
  }
  private def encoded(values: Seq[DedupGroupAnnotation]): JValue = parse(JsonProtocol.serialize(values))
  private def decode(doc: JValue, key: String, circuit: String): Seq[DedupGroupAnnotation] = {
    val values = (doc \ key) match {
      case a: JArray => JsonProtocol.deserialize(canonical(a)).map {
        case a: DedupGroupAnnotation if a.target.circuit == circuit && name(a.target.module) &&
            a.group != null && a.group.nonEmpty && !a.group.contains('\u0000') => a
        case _ => fail("invalid historical group")
      }
      case _ => fail("invalid historical groups")
    }
    if (values.map(_.target).distinct.size != values.size) fail("duplicate historical group")
    if (canonical(encoded(values)) != canonical(doc \ key)) fail("noncanonical historical group fields")
    values
  }
  private def document(body: String): JValue = {
    val doc = parse(body)
    if (canonical(doc) != body || (doc \ "version") != JInt(1) || (doc \ "policy") != JString(policy))
      fail("invalid canonical group policy")
    if (!name(text(doc, "circuit"))) fail("invalid historical circuit")
    doc
  }
  def context(a: DedupGroupPolicyAnnotation): JValue = {
    val doc = document(a.body)
    fields(doc, Set("version", "policy", "circuit", "input_sha256", "input_annotations_sha256", "groups"))
    Seq("input_sha256", "input_annotations_sha256").foreach(hash(doc, _))
    if (decode(doc, "groups", text(doc, "circuit")).isEmpty) fail("empty input policy")
    doc
  }
  def context(a: DedupGroupResultAnnotation): JValue = {
    val doc = document(a.body)
    fields(doc, Set("version", "policy", "circuit", "policy_sha256", "input_sha256", "output_sha256",
      "input_annotations_sha256", "output_annotations_sha256", "groups", "output_groups", "module_renames", "renames"))
    Seq("policy_sha256", "input_sha256", "output_sha256", "input_annotations_sha256", "output_annotations_sha256")
      .foreach(hash(doc, _))
    val circuit = text(doc, "circuit")
    val before = decode(doc, "groups", circuit).map(a => a.target.module -> a.group).toMap
    val after = decode(doc, "output_groups", circuit).map(a => a.target.module -> a.group).toMap
    val rows = (doc \ "module_renames") match { case JArray(xs) if xs.nonEmpty => xs; case _ => fail("missing module map") }
    val pairs = rows.map { r =>
      fields(r, Set("from", "to"))
      val from = text(r, "from"); val to = text(r, "to")
      if (!name(from) || !name(to)) fail("invalid module map")
      from -> to
    }
    if (pairs.map(_._1).distinct.size != pairs.size || !before.keySet.subsetOf(pairs.map(_._1).toSet) ||
        !after.keySet.subsetOf(pairs.map(_._2).toSet) || !pairs.contains(circuit -> circuit) ||
        pairs.exists { case (from, to) => before.get(from) != after.get(to) } ||
        before.toSeq.map { case (from, g) => pairs.toMap.apply(from) -> g }.toSet != after.toSet ||
        text(doc, "renames").isEmpty) fail("cross-group, incomplete or aliased dedup history")
    doc
  }
  def context(a: DedupGroupRetirementAnnotation): JValue = {
    val doc = document(a.body)
    fields(doc, Set("version", "policy", "circuit", "policy_sha256", "result_sha256", "input_sha256", "groups"))
    Seq("policy_sha256", "result_sha256", "input_sha256").foreach(hash(doc, _))
    decode(doc, "groups", text(doc, "circuit"))
    doc
  }
  private def base(state: CircuitState): List[(String, JValue)] = List(
    "version" -> JInt(1), "policy" -> JString(policy), "circuit" -> JString(state.circuit.main))

  /** Called at stage entry, before ANY ordinary FIRRTL 1.6 dedup can run. */
  def prepare(state: CircuitState): CircuitState = {
    if (state.annotations.exists {
      case _: DedupGroupPolicyAnnotation | _: DedupGroupResultAnnotation | _: DedupGroupRetirementAnnotation => true
      case _ => false
    }) fail("unexpected history at stage input")
    val values = groups(state)
    if (values.isEmpty) return state
    if (state.annotations.count(_ == NoCircuitDedupAnnotation) > 1) fail("duplicate generic dedup suppression")
    val history = DedupGroupPolicyAnnotation(canonical(JObject(base(state) ++ List(
      "input_sha256" -> JString(sha256(state.circuit.serialize)),
      "input_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(state.annotations))),
      "groups" -> encoded(values)))))
    context(history)
    state.copy(annotations = state.annotations ++
      (if (state.annotations.contains(NoCircuitDedupAnnotation)) Nil else Seq(NoCircuitDedupAnnotation)) :+ history)
  }

  /** The native pass still runs once. Native NoDedup and prior DupedResult
    * semantics remain in force; only structurally equal same-group modules
    * merge. Remove group annotations from native instanceification and rebuild
    * the exact module assignments from the pass's complete module map.
    */
  def run(state: CircuitState): CircuitState = {
    val policies = state.annotations.collect { case a: DedupGroupPolicyAnnotation => a }
    if (policies.isEmpty) {
      if (state.annotations.exists(_.isInstanceOf[DedupGroupAnnotation])) fail("unprepared group constraints")
      if (state.annotations.exists {
        case _: DedupGroupResultAnnotation | _: DedupGroupRetirementAnnotation => true
        case _ => false
      }) fail("orphaned group history")
      val (disabled, other) = state.annotations.partition(_ == NoCircuitDedupAnnotation)
      val result = (new firrtl.transforms.DedupModules).runTransform(state.copy(annotations = other))
      return result.copy(annotations = result.annotations ++ disabled)
    }
    if (policies.size != 1 || !state.annotations.contains(NoCircuitDedupAnnotation) || state.annotations.exists {
      case _: DedupGroupResultAnnotation | _: DedupGroupRetirementAnnotation => true
      case _ => false
    }) fail("missing suppression or repeated forced dedup")
    context(policies.head)
    val values = groups(state)
    val assignments = values.map(a => a.target.module -> a.group).toMap
    val input = state.copy(annotations = state.annotations.filterNot(a =>
      a == NoCircuitDedupAnnotation || a.isInstanceOf[DedupGroupAnnotation]))
    val pass = new CheckpointGroupDedupModules(assignments)
    val native = pass.runTransform(input)
    val outputGroups = values.map { a => a.copy(target = a.target.copy(module = pass.moduleRenames(a.target.module))) }
      .distinct.sortBy(_.target.serialize)
    val result = native.copy(annotations = native.annotations ++ outputGroups :+ NoCircuitDedupAnnotation)
    groups(result)
    val rows = pass.moduleRenames.toSeq.sortBy(_._1).map { case (from, to) =>
      JObject(List("from" -> JString(from), "to" -> JString(to)))
    }
    val history = DedupGroupResultAnnotation(canonical(JObject(base(state) ++ List(
      "policy_sha256" -> JString(sha256(policies.head.body)),
      "input_sha256" -> JString(sha256(state.circuit.serialize)),
      "output_sha256" -> JString(sha256(result.circuit.serialize)),
      "input_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(state.annotations))),
      "output_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(result.annotations))),
      "groups" -> encoded(values), "output_groups" -> encoded(outputGroups),
      "module_renames" -> JArray(rows.toList), "renames" -> JString(pass.renameHistory)))))
    context(history)
    result.copy(annotations = result.annotations :+ history)
  }

  /** Retire live constraints immediately after the forced dedup pass. All
    * remaining ordinary dedup is disabled and another forced pass rejects the
    * consumed receipt. The original and actual-pass history remain immutable
    * through linking, banking and final emission.
    */
  def retire(state: CircuitState): CircuitState = {
    if (state.annotations.exists(_.isInstanceOf[DedupGroupRetirementAnnotation])) {
      validateFinal(state.annotations)
      return state
    }
    val policies = state.annotations.collect { case a: DedupGroupPolicyAnnotation => a }
    val results = state.annotations.collect { case a: DedupGroupResultAnnotation => a }
    val values = groups(state)
    if (policies.isEmpty && results.isEmpty && values.isEmpty) return state
    if (policies.size != 1 || results.size != 1 || !state.annotations.contains(NoCircuitDedupAnnotation) ||
        state.form != firrtl.LowForm || state.annotations.exists(_.isInstanceOf[DedupGroupRetirementAnnotation]))
      fail("cannot retire unconsumed group constraints")
    context(policies.head); context(results.head)
    if ((context(results.head) \ "policy_sha256") != JString(sha256(policies.head.body))) fail("changed group policy")
    val history = DedupGroupRetirementAnnotation(canonical(JObject(base(state) ++ List(
      "policy_sha256" -> JString(sha256(policies.head.body)), "result_sha256" -> JString(sha256(results.head.body)),
      "input_sha256" -> JString(sha256(state.circuit.serialize)), "groups" -> encoded(values)))))
    context(history)
    state.copy(annotations = state.annotations.filterNot(_.isInstanceOf[DedupGroupAnnotation]) :+ history)
  }

  def validatePreserved(expected: AnnotationSeq, actual: AnnotationSeq): Unit = {
    def policies(a: AnnotationSeq) = a.collect { case a: DedupGroupPolicyAnnotation => a }
    if (policies(expected) != policies(actual)) fail("input group policy changed or missing")
    if (policies(expected).nonEmpty && !actual.contains(NoCircuitDedupAnnotation)) fail("generic dedup suppression lost")
  }

  /** Audit the linked immutable history together, including at both bank
    * emission boundaries. A valid-looking isolated receipt is insufficient.
    */
  def validateHistory(annotations: AnnotationSeq): Unit = {
    val policies = annotations.collect { case a: DedupGroupPolicyAnnotation => a }
    val results = annotations.collect { case a: DedupGroupResultAnnotation => a }
    val retired = annotations.collect { case a: DedupGroupRetirementAnnotation => a }
    if (policies.isEmpty && results.isEmpty && retired.isEmpty) return
    if (policies.size != 1 || results.size != 1 || retired.size != 1) fail("incomplete or duplicate final group history")
    context(policies.head)
    val r = context(results.head); val t = context(retired.head)
    if ((r \ "policy_sha256") != JString(sha256(policies.head.body)) ||
        (t \ "policy_sha256") != JString(sha256(policies.head.body)) ||
        (t \ "result_sha256") != JString(sha256(results.head.body))) fail("changed final group history binding")
  }
  def validateFinal(annotations: AnnotationSeq): Unit = {
    validateHistory(annotations)
    if (annotations.exists(_.isInstanceOf[DedupGroupAnnotation])) fail("live group constraint at final boundary")
    if (annotations.exists(_.isInstanceOf[DedupGroupPolicyAnnotation]) &&
        !annotations.contains(NoCircuitDedupAnnotation)) fail("final generic dedup suppression lost")
  }
}
