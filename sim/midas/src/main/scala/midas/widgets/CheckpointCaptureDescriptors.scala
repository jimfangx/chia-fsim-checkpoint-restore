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
  def fromGenerated(result: GeneratedStateBankAccess.Result, manifest: String): CheckpointCaptureDescriptors = {
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
    check(text(binding, "scope") == "all_reachable_firrtl_state_only", "unsupported coverage scope")
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
      simulatorManifest: String, plan: String, dependencies: String): CheckpointCaptureDescriptors = {
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
      })) ++ flags))
    val binding = canonical(JObject(List(
      "descriptor_binding_version" -> JInt(1), "manifest_sha256" -> JString(sha256(manifest)),
      "functional_manifest" -> parse(functional.manifest), "functional_binding" -> parse(functional.binding),
      "physical_bank_binding" -> physical,
      "access_clock" -> JString("ungated_common_source"),
      "reset_contract" -> JString("same_reset_as_sampler_transport_and_bank_endpoints")) ++ flags))
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
