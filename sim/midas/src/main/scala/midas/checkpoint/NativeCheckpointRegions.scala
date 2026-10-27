// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance}
import firrtl.annotations.{InstanceTarget, JsonProtocol, ModuleTarget}
import firrtl.ir._
import midas.widgets.CheckpointCaptureDescriptors
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Install independently reconstructed banks into disjoint executions of ONE
  * original host. Shared ancestors carry separate sidebands; original execution
  * and clock connections are never duplicated. A downstream common low-phase
  * controller must still freeze all reset, external, bridge and memory owners.
  */
object NativeCheckpointRegions {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, host: CircuitState, targets: Seq[InstanceTarget],
      regions: Seq[NativeCheckpointInPlaceContext.Result], descriptors: CheckpointCaptureDescriptors,
      routing: CheckpointRegionRouting.Result, binding: String, inventory: Option[ReservedCheckpointInventory] = None)
  def prefix(index: Int): String = NativeCheckpointInPlace.prefix + s"region${index}_"
  private def check(ok: Boolean, why: String): Unit = require(ok, "native checkpoint regions: " + why)
  private def json(as: firrtl.AnnotationSeq): String = JsonProtocol.serialize(as.distinct.sortBy(_.serialize))

  /** FIRRTL text omits expression types, kinds and flows. Report the first
    * concrete AST difference without treating equal text as equal typed IR.
    */
  private[checkpoint] def firstStructuralDifference(original: Any, generated: Any): String = {
    def value(x: Any): String = if (x == null) "null" else
      x.getClass.getSimpleName.stripSuffix("$") + ":" + x.toString.take(180)
    def first(a: Any, b: Any, path: String): String = {
      if (a == b) path + ": equal"
      else (a, b) match {
        case (left: Seq[_], right: Seq[_]) =>
          if (left.size != right.size) s"$path.size: ${left.size} -> ${right.size}"
          else {
            val i = left.indices.find(i => left(i) != right(i)).get
            first(left(i), right(i), s"$path[$i]")
          }
        case (left: Product, right: Product) if left.getClass == right.getClass && left.productArity == right.productArity =>
          val i = (0 until left.productArity).find(i => left.productElement(i) != right.productElement(i))
          i.map(index => first(left.productElement(index), right.productElement(index),
            path + "." + left.productElementName(index))).getOrElse(s"$path: ${value(a)} -> ${value(b)}")
        case _ => s"$path: ${value(a)} -> ${value(b)}"
      }
    }
    first(original, generated, "module")
  }

  def generate(host: CircuitState, targets: Seq[InstanceTarget],
      inventory: Option[ReservedCheckpointInventory] = None): Result = {
    check(host.form == LowForm, "resolved LowForm required")
    inventory.foreach(_.validateSource(host))
    check(targets.nonEmpty && targets.size <= 255, "invalid region count")
    val paths = targets.map(_.asPath.map(_._1.value))
    check(!paths.indices.exists(i => paths.indices.exists(j => i != j && paths(i).startsWith(paths(j)))),
      "overlapping selected executions")
    val regions = targets.zipWithIndex.map { case (t, i) =>
      NativeCheckpointInPlaceContext.generate(host, t, prefix(i), inventory)
    }
    val clocks = regions.map(r => parse(r.native.binding) \ "enclosing_raw_clock")
    check(clocks.distinct.size == 1, "selected executions do not share a proven raw clock")
    val descriptors = regions.zip(targets).map { case (r, t) =>
      CheckpointCaptureDescriptors.fromNativeInPlace(host.copy(annotations = host.annotations.filterNot(
        SimulatorBankProbeContext.accepts)), t, r.reference, r.manifest, r.plan, r.dependencies, inventory)
    }
    val (combined, routing) = CheckpointCaptureDescriptors.fromNativeRegions(descriptors)
    val originals = host.circuit.modules.map(m => m.name -> m).toMap
    check(originals.size == host.circuit.modules.size, "duplicate original definitions")

    // Each independent bank generator specializes its children with local
    // names. Namespace those DEFINITIONS and all their instance/annotation
    // references before combining; equal local names need not mean equal RTL.
    val generatedAnnotations = scala.collection.mutable.ArrayBuffer.empty[firrtl.annotations.Annotation]
    val generatedNames = scala.collection.mutable.Set.empty[String] ++ originals.keys
    val namespaced = regions.zipWithIndex.map { case (r, index) =>
      val names = r.state.circuit.modules.filterNot(m => originals.contains(m.name)).map { m =>
        val name = s"CheckpointRegion${index}_${m.name}"
        check(!generatedNames(name), "generated definition collision " + name)
        generatedNames += name
        m.name -> name
      }.toMap
      def statement(s: Statement): Statement = s match {
        case i: WDefInstance => i.copy(module = names.getOrElse(i.module, i.module))
        case x => x.mapStmt(statement)
      }
      val modules = r.state.circuit.modules.map {
        case m: Module => m.copy(name = names.getOrElse(m.name, m.name), body = statement(m.body))
        case m: ExtModule => m.copy(name = names.getOrElse(m.name, m.name))
      }
      val renames = firrtl.RenameMap()
      names.foreach { case (before, after) => renames.record(ModuleTarget(host.circuit.main, before),
        ModuleTarget(host.circuit.main, after)) }
      generatedAnnotations ++= r.native.generatedAnnotations.flatMap(_.update(renames))
      modules.map(m => m.name -> m).toMap
    }
    val selected = targets.map(_.asPath.last._2.value).toSet
    val ancestors = targets.map(t => (Seq(host.circuit.main) ++ t.asPath.dropRight(1).map(_._2.value)).toSet)
    val modules = host.circuit.modules.map { original =>
      val changed = namespaced.zipWithIndex.collect { case (ms, index) if ms(original.name) != original =>
        index -> ms(original.name)
      }
      if (changed.isEmpty) original
      else if (selected(original.name)) {
        check(changed.size == 1, "selected definition modified by another region")
        changed.head._2
      } else {
        val m = original.asInstanceOf[Module]
        val additions = changed.map { case (i, definition) =>
          if (!ancestors(i)(m.name)) check(false, "changed unrelated original definition " + m.name +
            s" in region $i; " + firstStructuralDifference(m, definition))
          val next = definition.asInstanceOf[Module]
          check(next.ports.take(m.ports.size) == m.ports &&
            next.ports.drop(m.ports.size).forall(_.name.startsWith(prefix(i))), "changed original interface")
          def flat(s: Statement): Seq[Statement] = s match {
            case Block(xs) => xs.flatMap(flat)
            case x => Seq(x)
          }
          val before = flat(m.body); val after = flat(next.body)
          check(after.take(before.size).map(_.serialize) == before.map(_.serialize),
            "changed original ancestor body " + m.name)
          val extra = after.drop(before.size)
          (next.ports.drop(m.ports.size), extra)
        }
        val ports = m.ports ++ additions.flatMap(_._1)
        check(ports.map(_.name).distinct.size == ports.size, "colliding sideband ports")
        m.copy(ports = ports, body = Block(Seq(m.body) ++ additions.flatMap(_._2)))
      }
    } ++ namespaced.flatMap(_.values.filterNot(m => originals.contains(m.name))).sortBy(_.name)
    val circuit = host.circuit.copy(modules = modules)
    val working = firrtl.passes.ToWorkingIR.run(circuit)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val bare = CircuitState(flowed, LowForm)
    val before = GeneratedExternalOwners.instances(host, host.circuit.main)
    val after = GeneratedExternalOwners.instances(bare, host.circuit.main).toMap
    val storage = host.copy(annotations = host.annotations.filterNot(SimulatorBankProbeContext.accepts))
    val clones = before.collect { case (path, m: Module) =>
      val emitted = after.getOrElse(path, throw new IllegalArgumentException("native checkpoint regions: lost execution " + path))
      check(emitted.isInstanceOf[Module], "changed concrete owner " + path)
      GeneratedStateBankAnnotations.Clone(m.name, path, emitted.name, m)
    }
    val (projected, projection) = GeneratedStateBankAnnotations.translate(storage, clones)
    val context = host.annotations.filter(SimulatorBankProbeContext.accepts)
    val annotations = (projected ++ generatedAnnotations).filterNot(_.isInstanceOf[firrtl.transforms.CombinationalPath]).distinct
    val state = bare.copy(annotations = annotations ++ context)
    val finalClones = after.toSeq.collect { case (path, m: Module) =>
      GeneratedStateBankAnnotations.Clone(m.name, path, m.name, m)
    }
    val (validated, _) = GeneratedStateBankAnnotations.translate(state.copy(annotations = annotations), finalClones)
    check(validated.toSet == annotations.toSet, "post-graft annotation targets changed")
    val snapshot = SimulatorBankProbeContext.snapshot(context, host.circuit.serialize, state.circuit.serialize)
    val binding = canonical(JObject(List(
      "regions_version" -> JInt(1), "scope" -> JString("existing_disjoint_native_executions_only"),
      "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "host_annotations_sha256" -> JString(sha256(json(host.annotations))),
      "instrumented_sha256" -> JString(sha256(state.circuit.serialize)),
      "instrumented_annotations_sha256" -> JString(sha256(json(state.annotations))),
      "raw_clock" -> clocks.head, "context" -> parse(snapshot), "annotation_projection" -> projection,
      "descriptor_manifest_sha256" -> JString(combined.manifestSha256),
      "descriptor_binding_sha256" -> JString(combined.bindingSha256),
      "routing_sha256" -> JString(sha256(routing.binding)),
      "regions" -> JArray(regions.zipWithIndex.map { case (r, i) => JObject(List(
        "index" -> JInt(i), "target" -> JString(targets(i).serialize), "sideband_prefix" -> JString(prefix(i)),
        "context_binding_sha256" -> JString(sha256(r.binding)))) }.toList),
      "original_execution_duplicated" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "deployable_capture_image" -> JBool(false), "metasim_restore_supported" -> JBool(false)) ++
      inventory.toList.flatMap(i => List("unsupported_external_owners" -> i.unsupportedOwners,
        "reserved_inventory_binding_sha256" -> JString(sha256(i.binding))))))
    Result(state, host, targets, regions, combined, routing, binding, inventory)
  }

  /** Reconstruct from the original, including annotation/context semantics;
    * caller-rehashed circuits or metadata cannot establish coverage. */
  def validate(result: Result): Unit = {
    val rebuilt = generate(result.host, result.targets, result.inventory)
    def sameState(a: CircuitState, b: CircuitState): Boolean = a.form == b.form && a.renames == b.renames &&
      a.circuit == b.circuit && a.annotations.toVector == b.annotations.toVector
    check(sameState(result.state, rebuilt.state) &&
      result.binding == rebuilt.binding && result.descriptors.manifest == rebuilt.descriptors.manifest &&
      result.descriptors.binding == rebuilt.descriptors.binding &&
      result.descriptors.entries == rebuilt.descriptors.entries &&
      result.descriptors.physicalClockGates == rebuilt.descriptors.physicalClockGates && result.routing == rebuilt.routing &&
      result.regions.size == rebuilt.regions.size && result.regions.zip(rebuilt.regions).forall { case (a, b) =>
        a.binding == b.binding && a.context == b.context && sameState(a.state, b.state) && sameState(a.host, b.host) &&
          sameState(a.reference, b.reference) && a.manifest == b.manifest && a.plan == b.plan && a.dependencies == b.dependencies &&
          a.native.binding == b.native.binding && a.native.identity == b.native.identity &&
          a.native.annotationProjection == b.native.annotationProjection && sameState(a.native.state, b.native.state) &&
          a.native.generatedAnnotations.toVector == b.native.generatedAnnotations.toVector
      }, "changed source, network, context or identity")
  }
}
