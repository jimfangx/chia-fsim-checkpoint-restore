// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm}
import firrtl.annotations.InstanceTarget
import org.json4s._
import org.json4s.native.JsonMethods.parse
import midas.widgets.CheckpointCaptureDescriptors

/** Reconcile an in-place bank's local selectors with the original enclosing
  * simulator's StateIDs. Selectors and descriptor order remain LOCAL to the
  * selected endpoint. Global inventory order is never a bank address.
  *
  * Unselected functional and physical owners are an explicit coverage deficit;
  * this does not turn a subtree into a complete simulator checkpoint.
  */
object NativeCheckpointIdentity {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(manifest: String, binding: String, entries: Seq[StateAccessEntry],
      simulatorManifest: String, clockPlan: String, dependencies: String, physicalClockGates: Int)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "Native in-place identity: " + why)
  private def text(row: JValue, key: String): String = (row \ key) match {
    case JString(s) => s
    case _ => throw new IllegalArgumentException("Native in-place identity: missing " + key)
  }
  private def rows(doc: String, key: String): List[JValue] = (parse(doc) \ key) match {
    case JArray(xs) => xs
    case _ => throw new IllegalArgumentException("Native in-place identity: missing " + key)
  }

  def discover(host: CircuitState, target: InstanceTarget, reference: CircuitState,
      manifest: String, plan: String, dependencies: String,
      inventory: Option[ReservedCheckpointInventory] = None): Result = {
    check(host.form == LowForm && reference.form == LowForm, "resolved LowForm required")
    check(target.circuit == host.circuit.main && target.module == host.circuit.main,
      "target must be rooted at enclosing circuit")
    val path = target.asPath.map { case (i, m) => i.value -> m.value }
    check(path.nonEmpty && path.last._2 == reference.circuit.main, "selected root mismatch")
    val live = GeneratedExternalOwners.instances(host, host.circuit.main).toMap
    var scope = host.circuit.main
    path.foreach { case (instance, module) =>
      scope += "." + instance
      check(live.get(scope).exists(_.name == module), "stale concrete target path " + scope)
    }
    check(live.values.count(_.name == reference.circuit.main) == 1, "ambiguous selected execution")
    val definitions = host.circuit.modules.map(m => m.name -> m).toMap
    check(definitions.size == host.circuit.modules.size, "duplicate enclosing definition")
    GeneratedExternalOwners.instances(reference, reference.circuit.main).map(_._2).distinctBy(_.name).foreach { m =>
      check(definitions.get(m.name).contains(m), "stale executing definition " + m.name)
    }
    val selectedRoot = reference.circuit.main
    def rebase(local: String): String = {
      check(local.startsWith(selectedRoot + "."), "path outside selected root " + local)
      scope + local.drop(selectedRoot.length)
    }

    // Reconstruct both sides from actual FIRRTL, including native topology and
    // CE dependencies. No caller-supplied ID, selector or coverage list is trusted.
    val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, dependencies)
    val descriptors = CheckpointCaptureDescriptors.fromNativeGenerated(banks, reference, manifest, plan, dependencies)
    val analysis = inventory.map(_.analysisFor(host)).getOrElse(host)
    val globalManifest = SimulatorStateInventory.discover(analysis)
    val globalPlan = PhysicalClockCutPlan.discover(analysis, globalManifest)
    val globalDependencies = PhysicalClockCutDependencies.discover(analysis, globalManifest, globalPlan)
    val external = inventory.toList.flatMap(i => List(
      "unsupported_external_owners" -> i.unsupportedOwners,
      "reserved_inventory_binding_sha256" -> JString(sha256(i.binding))))
    val globalFunctional = rows(globalManifest, "state").filterNot(r =>
      (r \ "adapter") == JString("metasim-bufgce-v1"))
    val globalPhysical = rows(globalPlan, "physical_gates")
    val byPath = globalFunctional.map(r => text(r, "hierarchy") -> r).toMap
    val byLatch = globalPhysical.map(r => text(r, "logical_latch_hierarchy") -> r).toMap
    check(byPath.size == globalFunctional.size && byLatch.size == globalPhysical.size, "duplicate global owner")
    val functionalRows = rows(banks.binding, "state")
    val logicalRows = (parse(descriptors.binding) \ "functional_manifest" \ "state").asInstanceOf[JArray].arr
    val localPhysical = rows(plan, "physical_gates")
    val functional = functionalRows.zip(logicalRows).zipWithIndex.map { case ((bound, logical), ordinal) =>
      val local = text(logical, "hierarchy")
      val globalPath = rebase(local)
      val global = byPath.getOrElse(globalPath,
        throw new IllegalArgumentException("Native in-place identity: missing global storage " + globalPath))
      val id = SimulatorStateInventory.stateID(globalPath, text(logical, "kind"))
      val expected = logical merge JObject(List("hierarchy" -> JString(globalPath), "state_id" -> JString(id)))
      check(global == expected, "global storage shape/adapter/identity mismatch " + globalPath)
      val entry = descriptors.entries(ordinal)
      check(entry.stateID == text(logical, "state_id"), "local functional descriptor order mismatch")
      (entry.copy(stateID = id), JObject(List(
        "ordinal" -> JInt(ordinal), "domain" -> JString("functional"),
        "selected_state_id" -> JString(entry.stateID), "state_id" -> JString(id),
        "selected_hierarchy" -> JString(local), "hierarchy" -> JString(globalPath),
        "storage_hierarchy" -> JString(rebase(text(bound, "storage_hierarchy"))),
        "bank" -> JInt(entry.bank), "slot" -> JInt(entry.slot),
        "width" -> JInt(entry.width), "depth" -> JInt(entry.depth), "logical_state" -> global)))
    }
    val physical = localPhysical.zipWithIndex.map { case (local, index) =>
      val localPath = text(local, "logical_latch_hierarchy")
      val globalPath = rebase(localPath)
      val global = byLatch.getOrElse(globalPath,
        throw new IllegalArgumentException("Native in-place identity: missing global physical owner " + globalPath))
      check((local \ "physical_contract") == (global \ "physical_contract") &&
        rebase(text(local, "hierarchy")) == text(global, "hierarchy"), "physical owner mismatch")
      val ordinal = functional.size + index
      val entry = descriptors.entries(ordinal)
      check(entry.stateID == text(local, "logical_latch_state_id") && entry.bank == 255 && entry.slot == index,
        "local physical descriptor order mismatch")
      val id = text(global, "logical_latch_state_id")
      (entry.copy(stateID = id), JObject(List(
        "ordinal" -> JInt(ordinal), "domain" -> JString("physical_ce"),
        "selected_state_id" -> JString(entry.stateID), "state_id" -> JString(id),
        "selected_hierarchy" -> JString(localPath), "hierarchy" -> JString(globalPath),
        "bank" -> JInt(entry.bank), "slot" -> JInt(entry.slot),
        "width" -> JInt(1), "depth" -> JInt(1), "ce_bit" -> JInt(index),
        "global_physical_index" -> (global \ "index"),
        "metasim_output_state_id" -> (global \ "metasim_output_state_id"),
        "restore_adapter" -> JString("unsupported"))))
    }
    val mapped = functional ++ physical
    val entries = mapped.map(_._1).toVector
    val covered = mapped.map(x => text(x._2, "hierarchy")).toSet
    val selectedFunctional = globalFunctional.filter(r => text(r, "hierarchy").startsWith(scope + "."))
    val selectedPhysical = globalPhysical.filter(r => text(r, "logical_latch_hierarchy").startsWith(scope + "."))
    check(selectedFunctional.size == functional.size && selectedPhysical.size == physical.size &&
      entries.map(_.stateID).distinct.size == entries.size && covered.size == entries.size,
      "incomplete or aliased selected coverage")
    val uncoveredFunctional = globalFunctional.filterNot(r => covered(text(r, "hierarchy")))
    val uncoveredPhysical = globalPhysical.filterNot(r => covered(text(r, "logical_latch_hierarchy")))
    val flags = List("complete_fpga_capture", "deployable_capture_image",
      "production_freeze_owners_bound", "metasim_restore_supported").map(_ -> JBool(false))
    val projected = canonical(JObject(List(
      "identity_version" -> JInt(1), "scope" -> JString("selected_inplace_global_identities_only"),
      "target" -> JString(target.serialize), "selected_scope" -> JString(scope),
      "selector_namespace" -> JString("selected_endpoint_local_order_not_global_inventory_order"),
      "state" -> JArray(mapped.map(_._2).toList),
      "uncovered_functional_state" -> JArray(uncoveredFunctional),
      "uncovered_physical_gates" -> JArray(uncoveredPhysical),
      "global_functional_entries" -> JInt(globalFunctional.size),
      "global_physical_gates" -> JInt(globalPhysical.size),
      "physical_clock_gates" -> JInt(physical.size),
      "external_runtime_required" -> JBool(true)) ++ flags ++ external))
    val binding = canonical(JObject(List(
      "identity_binding_version" -> JInt(1), "manifest_sha256" -> JString(sha256(projected)),
      "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "reference_sha256" -> JString(sha256(reference.circuit.serialize)),
      "global_simulator_manifest_sha256" -> JString(sha256(globalManifest)),
      "global_clock_plan_sha256" -> JString(sha256(globalPlan)),
      "global_dependencies_sha256" -> JString(sha256(globalDependencies)),
      "selected_descriptor_manifest_sha256" -> JString(descriptors.manifestSha256),
      "selected_descriptor_binding_sha256" -> JString(descriptors.bindingSha256)) ++ flags ++ external))
    Result(projected, binding, entries, globalManifest, globalPlan, globalDependencies, physical.size)
  }
}
