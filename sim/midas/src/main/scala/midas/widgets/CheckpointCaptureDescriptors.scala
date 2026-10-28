// See LICENSE for license details.
package midas.widgets

import chisel3._
import firrtl.CircuitState
import midas.checkpoint.{GeneratedStateBankAccess, PhysicalClockStateBank, SemanticStateInventory, StateAccessEntry}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** One immutable source for the synthesized ROM and generated host metadata.
  * Construction requires a complete generated bank result and its independent
  * inventory, including the exact instrumented circuit. This describes FIRRTL
  * storage only; it does not establish all-owner or CPU checkpoint coverage.
  */
final class CheckpointCaptureDescriptors private (
    val manifestSha256: String, val bindingSha256: String, val entries: Seq[StateAccessEntry],
    val manifest: String, val binding: String, val physicalClockGates: Int) {
  def cppManifest: String = {
    // The MMIO/transport ABI interprets the digest as a little-endian integer.
    val bytes = manifestSha256.grouped(2).toSeq.reverse.map(b => s"0x$b").mkString(", ")
    val rows = entries.map(e => s"{${e.bank}U, ${e.slot}U, ${e.width}U, ${e.depth}U}").mkString(", ")
    s"checkpoint_capture_manifest_t{{{$bytes}}, {$rows}}"
  }
}

object CheckpointCaptureDescriptors {
  /** Compose independently reconstructed regions of the SAME original host.
    * The returned ROM uses router-global selectors and enclosing StateIDs.
    * Missing owners remain explicit; this does not bind the shared freeze,
    * concatenate the physical CE wires, or authorize a checkpoint image.
    */
  def fromNativeRegions(regions: Seq[CheckpointCaptureDescriptors]):
      (CheckpointCaptureDescriptors, midas.checkpoint.CheckpointRegionRouting.Result) = {
    def check(ok: Boolean, why: String): Unit = require(ok, "checkpoint region descriptors: " + why)
    check(regions.nonEmpty, "no selected regions")
    val manifests = regions.map(r => parse(r.manifest))
    val bindings = regions.map(r => parse(r.binding))
    check(manifests.forall(m => Set[JValue](JString("selected_inplace_global_identities_only"),
      JString("selected_shell_global_identities_only"))(m \ "scope")),
      "regions must use independently reconstructed enclosing identities")
    Seq("host_sha256", "global_simulator_manifest_sha256", "global_clock_plan_sha256",
      "global_dependencies_sha256").foreach { key =>
      check((bindings.head \ key).isInstanceOf[JString] && bindings.map(_ \ key).distinct.size == 1,
        "different original host or common clock cut: " + key)
    }
    val scopes = manifests.map(m => (m \ "selected_scope").asInstanceOf[JString].s)
    check(!scopes.indices.exists(i => scopes.indices.exists(j => i != j &&
      (scopes(i) == scopes(j) || (scopes(i).startsWith(scopes(j) + ".") &&
        (manifests(j) \ "ownership_scope") != JString("local_storage_only"))))), "overlapping selected executions")
    val routing = midas.checkpoint.CheckpointRegionRouting.allocate(regions.map(_.entries))
    def rows(m: JValue, key: String): List[JValue] = (m \ key).asInstanceOf[JArray].arr
    val originalRows = manifests.flatMap(rows(_, "state"))
    val covered = originalRows.map(r => r \ "state_id").toSet
    val localRows = manifests.zip(routing.regions).flatMap { case (m, r) => rows(m, "state").map { row =>
      val JInt(bank) = row \ "bank"; val JInt(slot) = row \ "slot"
      val physical = if (bank != 255) Nil else List("local_ce_bit" -> (row \ "ce_bit"),
        "ce_bit" -> JInt(slot + r.ceBase))
      row merge JObject(List("region" -> JInt(scopes.indexOf((m \ "selected_scope").asInstanceOf[JString].s)),
        "local_ordinal" -> (row \ "ordinal"),
        "local_bank" -> JInt(bank), "local_slot" -> JInt(slot),
        "bank" -> JInt(if (bank == 255) bank else bank + r.bankBase),
        "slot" -> JInt(if (bank == 255) slot + r.ceBase else slot)) ++ physical)
    }}
    val byID = localRows.map(row => (row \ "state_id") -> row).toMap
    check(byID.size == routing.entries.size, "duplicate or missing region identity")
    val state = routing.entries.zipWithIndex.map { case (e, index) =>
      val row = byID(JString(e.stateID))
      check((row \ "bank") == JInt(e.bank) && (row \ "slot") == JInt(e.slot) &&
        (row \ "width") == JInt(e.width) && (row \ "depth") == JInt(e.depth), "routed descriptor mismatch")
      row merge JObject(List("ordinal" -> JInt(index)))
    }
    val physical = manifests.flatMap(rows(_, "uncovered_physical_gates")).distinct.filterNot(r =>
      covered(r \ "logical_latch_state_id"))
    val functional = manifests.flatMap(rows(_, "uncovered_functional_state")).distinct.filterNot(r =>
      covered(r \ "state_id"))
    val externalRows = manifests.flatMap(m => (m \ "unsupported_external_owners") match {
      case JArray(xs) => xs
      case JNothing => Nil
      case _ => throw new IllegalArgumentException("native regions: malformed external owners")
    }).distinct
    val external = if (externalRows.isEmpty) Nil else List("unsupported_external_owners" -> JArray(externalRows.toList))
    val flags = List("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").map(_ -> JBool(false))
    val manifest = SemanticStateInventory.canonical(JObject(List(
      "descriptor_version" -> JInt(1), "scope" -> JString("selected_native_regions_only"),
      "state" -> JArray(state.toList), "uncovered_functional_state" -> JArray(functional.toList),
      "uncovered_physical_gates" -> JArray(physical.toList),
      "physical_ce_order" -> JArray(scopes.zip(routing.regions).map { case (s, r) =>
        JObject(List("selected_scope" -> JString(s), "base" -> JInt(r.ceBase), "count" -> JInt(r.ceCount)))
      }.toList), "routing" -> parse(routing.binding)) ++ flags ++ external))
    val binding = SemanticStateInventory.canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("same_host_native_region_composition_only"),
      "manifest_sha256" -> JString(SemanticStateInventory.sha256(manifest)),
      "host_sha256" -> (bindings.head \ "host_sha256"),
      "regions" -> JArray(regions.map(r => JObject(List("manifest" -> parse(r.manifest),
        "binding" -> parse(r.binding), "manifest_sha256" -> JString(r.manifestSha256),
        "binding_sha256" -> JString(r.bindingSha256)))).toList)) ++ flags ++ external))
    (new CheckpointCaptureDescriptors(SemanticStateInventory.sha256(manifest), SemanticStateInventory.sha256(binding),
      routing.entries, manifest, binding, regions.map(_.physicalClockGates).sum), routing)
  }

  /** Use enclosing simulator StateIDs without changing the selected endpoint's
    * selectors, CE bit order or transfer order. Complete global deficits remain
    * in the manifest; this factory never grants capture/restore admission.
    */
  def fromNativeInPlace(host: CircuitState, target: firrtl.annotations.InstanceTarget,
      reference: CircuitState, simulatorManifest: String, plan: String,
      dependencies: String, inventory: Option[midas.checkpoint.ReservedCheckpointInventory] = None): CheckpointCaptureDescriptors = {
    val identity = midas.checkpoint.NativeCheckpointIdentity.discover(host, target, reference,
      simulatorManifest, plan, dependencies, inventory)
    fromNativeInPlaceIdentity(identity)
  }

  /** Rebuild local banks and reconcile their original global identities. */
  def fromNativeShell(shell: midas.checkpoint.NativeCheckpointShellStateAccess.Result,
      inventory: Option[midas.checkpoint.ReservedCheckpointInventory] = None): CheckpointCaptureDescriptors = {
    val identity = midas.checkpoint.NativeCheckpointShellIdentity.discover(shell, inventory)
    new CheckpointCaptureDescriptors(SemanticStateInventory.sha256(identity.manifest),
      SemanticStateInventory.sha256(identity.binding), shell.entries, identity.manifest, identity.binding, 0)
  }

  private def fromNativeInPlaceIdentity(identity: midas.checkpoint.NativeCheckpointIdentity.Result):
      CheckpointCaptureDescriptors = {
    // Only the reconstructed result above is accepted by this private path.
    val sha = SemanticStateInventory.sha256 _
    new CheckpointCaptureDescriptors(sha(identity.manifest), sha(identity.binding), identity.entries,
      identity.manifest, identity.binding, identity.physicalClockGates)
  }

  def fromGenerated(result: GeneratedStateBankAccess.Result, manifest: String): CheckpointCaptureDescriptors =
    fromGeneratedImpl(result, manifest, native = false)

  /** Native functional storage and physical CE identities from one original
    * circuit. Reconstruct the generated banks instead of trusting a rehashed
    * caller result. This is still descriptor composition, not freeze ownership
    * or admission of a deployable checkpoint image.
    */
  def fromNativeGenerated(result: GeneratedStateBankAccess.Result, reference: CircuitState,
      simulatorManifest: String, plan: String, dependencies: String): CheckpointCaptureDescriptors = {
    val inventory = GeneratedStateBankAccess.physicalFunctionalInventory(reference, simulatorManifest, plan, dependencies)
    val regenerated = GeneratedStateBankAccess.generatePhysicalCompanion(reference, simulatorManifest, plan, dependencies)
    require(result.binding == regenerated.binding && result.entries == regenerated.entries,
      "checkpoint descriptors: native banks do not match the original circuit")
    val functional = fromGeneratedImpl(result, inventory, native = true)
    composePhysicalClockState(functional, reference, simulatorManifest, plan, dependencies, sameReference = true)
  }

  private def fromGeneratedImpl(result: GeneratedStateBankAccess.Result, manifest: String,
      native: Boolean): CheckpointCaptureDescriptors = {
    GeneratedStateBankAccess.validateAnnotationBinding(result)
    def check(ok: Boolean, why: String): Unit = require(ok, s"checkpoint descriptors: $why")
    val doc = parse(manifest)
    val binding = parse(result.binding)
    def text(row: JValue, key: String): String = (row \ key) match {
      case JString(value) => value
      case _ => throw new IllegalArgumentException(s"checkpoint descriptors: missing $key")
    }
    def integer(row: JValue, key: String): Int = (row \ key) match {
      case JInt(value) if value.isValidInt => value.toInt
      case _ => throw new IllegalArgumentException(s"checkpoint descriptors: invalid $key")
    }
    def rows(value: JValue): List[JValue] = (value \ "state") match {
      case JArray(values) => values
      case _ => throw new IllegalArgumentException("checkpoint descriptors: missing state")
    }
    val chunked = integer(binding, "access_version") == 2 &&
      text(binding, "protocol") == "latched-banked64-chunked-v2"
    check(chunked || (integer(binding, "access_version") == 1 && text(binding, "protocol") == "latched-banked64-v1"),
      "unsupported generated bank protocol")
    check(text(binding, "scope") == (if (native) "native_functional_readout_companion_only" else
      "all_reachable_firrtl_state_only"), "unsupported coverage scope")
    val manifestHash = SemanticStateInventory.sha256(SemanticStateInventory.canonical(doc))
    check(text(binding, "manifest_sha256") == manifestHash, "stale or incompatible inventory")
    check(text(binding, "reference_sha256") == text(doc, "reference_sha256"), "reference circuit mismatch")
    check(text(binding, "instrumented_sha256") == SemanticStateInventory.sha256(result.state.circuit.serialize),
      "instrumented circuit mismatch")
    val expected = rows(doc)
    val actual = rows(binding)
    check(expected.nonEmpty && expected.size <= 65535 && actual.size == expected.size, "incomplete inventory")
    val entries = actual.zip(expected).map { case (bound, logical) =>
      Seq("state_id", "hierarchy").foreach(k => check(text(bound, k) == text(logical, k), s"state order/$k mismatch"))
      Seq("width", "depth").foreach(k => check(integer(bound, k) == integer(logical, k), s"state $k mismatch"))
      check(text(bound, "storage_hierarchy").nonEmpty, "missing emitted storage path")
      val width = integer(logical, "width"); val depth = integer(logical, "depth")
      val chunks = (width.toLong + 63) / 64
      val words = chunks * depth
      check(width > 0 && depth > 0 && words <= Int.MaxValue, "flattened depth exceeds supported transport ABI")
      if (chunked) {
        check(integer(bound, "chunks") == chunks && integer(bound, "transfer_width") == math.min(width, 64) &&
          integer(bound, "transfer_depth") == words, "incompatible chunk mapping")
      } else check(width <= 64, "wide state requires versioned chunk mapping")
      val entry = StateAccessEntry(text(bound, "state_id"), integer(bound, "bank"), integer(bound, "slot"),
        math.min(width, 64), words.toInt)
      entry
    }
    check(entries == result.entries, "bank result and descriptor binding disagree")
    check(entries.map(_.stateID) == entries.map(_.stateID).sorted &&
      entries.map(_.stateID).distinct.size == entries.size, "duplicate or unordered StateID")
    check(entries.map(e => (e.bank, e.slot)).distinct.size == entries.size, "duplicate selector")
    // This constructor proves FIRRTL functional storage only. Native CE
    // owners need a separate, validated composition; assigning a functional
    // bit the physical bank's selector cannot establish that ownership.
    check(entries.forall(_.bank != 255), "bank 255 is reserved for physical CE owners")
    new CheckpointCaptureDescriptors(manifestHash,
      SemanticStateInventory.sha256(SemanticStateInventory.canonical(binding)), entries.toVector,
      SemanticStateInventory.canonical(doc), SemanticStateInventory.canonical(binding), 0)
  }

  /** Compose descriptor identities, not freeze-owner admission. Functional
    * banks and physical CE samples may come from separate component circuits;
    * both references are retained and complete production coverage stays false.
    * Regenerate the endpoint from the resolved physical circuit instead of
    * trusting caller-supplied slots, StateIDs, hashes or endpoint bytes.
    */
  def withPhysicalClockState(functional: CheckpointCaptureDescriptors, physicalState: CircuitState,
      simulatorManifest: String, plan: String, dependencies: String): CheckpointCaptureDescriptors =
    composePhysicalClockState(functional, physicalState, simulatorManifest, plan, dependencies, sameReference = false)

  private def composePhysicalClockState(functional: CheckpointCaptureDescriptors, physicalState: CircuitState,
      simulatorManifest: String, plan: String, dependencies: String,
      sameReference: Boolean): CheckpointCaptureDescriptors = {
    import SemanticStateInventory.{canonical, sha256}
    def check(ok: Boolean, why: String): Unit = require(ok, s"checkpoint descriptors: $why")
    check(functional.physicalClockGates == 0, "physical descriptors already composed")
    val files = PhysicalClockStateBank.emit(physicalState, simulatorManifest, plan, dependencies)
    val physical = parse(files.head.body)
    val physicalRows = (physical \ "state").asInstanceOf[JArray].arr
    check(functional.entries.size.toLong + physicalRows.size <= 65535, "composed descriptor count exceeds transport ABI")
    val physicalEntries = physicalRows.zipWithIndex.map { case (row, index) =>
      val JString(id) = row \ "state_id"
      check((row \ "bank") == JInt(255) && (row \ "slot") == JInt(index) &&
        (row \ "ce_bit") == JInt(index) && (row \ "width") == JInt(1) &&
        (row \ "depth") == JInt(1), "invalid generated physical CE descriptor")
      StateAccessEntry(id, 255, index, 1, 1)
    }
    val entries = (functional.entries ++ physicalEntries).toVector
    check(entries.map(_.stateID).distinct.size == entries.size, "functional/physical StateID alias")
    check(entries.map(e => (e.bank, e.slot)).distinct.size == entries.size, "functional/physical selector alias")
    val functionalRows = functional.entries.map { e => JObject(List(
      "state_id" -> JString(e.stateID), "bank" -> JInt(e.bank), "slot" -> JInt(e.slot),
      "width" -> JInt(e.width), "depth" -> JInt(e.depth), "domain" -> JString("functional"))) }
    val flags = List("complete_fpga_capture", "deployable_capture_image",
      "production_freeze_owners_bound", "metasim_restore_supported").map(_ -> JBool(false))
    val shared = if (!sameReference) Nil else {
      val referenceHash = sha256(physicalState.circuit.serialize)
      check((parse(functional.binding) \ "reference_sha256") == JString(referenceHash) &&
        (parse(functional.manifest) \ "reference_sha256") == JString(referenceHash),
        "functional and physical reference circuits differ")
      List("same_reference_sha256" -> JString(referenceHash))
    }
    val manifest = canonical(JObject(List(
      "descriptor_version" -> JInt(1),
      "scope" -> JString("functional_and_physical_ce_descriptor_composition_only"),
      "functional_manifest_sha256" -> JString(functional.manifestSha256),
      "functional_binding_sha256" -> JString(functional.bindingSha256),
      "physical_bank_binding_sha256" -> JString(sha256(files.head.body)),
      "functional_entries" -> JInt(functional.entries.size),
      "physical_clock_gates" -> JInt(physicalEntries.size),
      "state" -> JArray(functionalRows.toList ++ physicalRows.map { row =>
        row merge JObject(List("domain" -> JString("physical_ce")))
      })) ++ shared ++ flags))
    val binding = canonical(JObject(List(
      "descriptor_binding_version" -> JInt(1), "manifest_sha256" -> JString(sha256(manifest)),
      "functional_manifest" -> parse(functional.manifest), "functional_binding" -> parse(functional.binding),
      "physical_bank_binding" -> physical,
      "access_clock" -> JString("ungated_common_source"),
      "reset_contract" -> JString("same_reset_as_sampler_transport_and_bank_endpoints")) ++ shared ++ flags))
    new CheckpointCaptureDescriptors(sha256(manifest), sha256(binding), entries,
      manifest, binding, physicalEntries.size)
  }
}

/** A combinational, bounded ROM. Invalid ordinals return an invalid all-zero
  * descriptor; they cannot alias a truncated valid index. The transport latches
  * a descriptor before issuing its first bank request.
  */
class CheckpointCaptureDescriptorROM(descriptors: CheckpointCaptureDescriptors) extends Module {
  val io = IO(new Bundle {
    val entry = Input(UInt(32.W))
    val valid = Output(Bool())
    val bank = Output(UInt(8.W))
    val slot = Output(UInt(16.W))
    val bitWidth = Output(UInt(16.W))
    val depth = Output(UInt(32.W))
  })
  val entries = descriptors.entries
  io.valid := io.entry < entries.size.U
  io.bank := Mux(io.valid, VecInit(entries.map(_.bank.U(8.W)))(io.entry), 0.U)
  io.slot := Mux(io.valid, VecInit(entries.map(_.slot.U(16.W)))(io.entry), 0.U)
  io.bitWidth := Mux(io.valid, VecInit(entries.map(_.width.U(16.W)))(io.entry), 0.U)
  io.depth := Mux(io.valid, VecInit(entries.map(_.depth.U(32.W)))(io.entry), 0.U)
}
