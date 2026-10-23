// See LICENSE for license details.
package midas.widgets

import chisel3._
import midas.checkpoint.{GeneratedStateBankAccess, SemanticStateInventory, StateAccessEntry}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** One immutable source for the synthesized ROM and generated host metadata.
  * Construction requires a complete generated bank result and its independent
  * inventory, including the exact instrumented circuit. This describes FIRRTL
  * storage only; it does not establish all-owner or CPU checkpoint coverage.
  */
final class CheckpointCaptureDescriptors private (
    val manifestSha256: String, val bindingSha256: String, val entries: Seq[StateAccessEntry]) {
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
    check(integer(binding, "access_version") == 1 && text(binding, "protocol") == "latched-banked64-v1",
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
      val entry = StateAccessEntry(text(bound, "state_id"), integer(bound, "bank"), integer(bound, "slot"),
        integer(bound, "width"), integer(bound, "depth"))
      check(entry.depth <= 65535, "depth exceeds transport ABI")
      entry
    }
    check(entries == result.entries, "bank result and descriptor binding disagree")
    check(entries.map(_.stateID) == entries.map(_.stateID).sorted &&
      entries.map(_.stateID).distinct.size == entries.size, "duplicate or unordered StateID")
    check(entries.map(e => (e.bank, e.slot)).distinct.size == entries.size, "duplicate selector")
    new CheckpointCaptureDescriptors(manifestHash,
      SemanticStateInventory.sha256(SemanticStateInventory.canonical(binding)), entries.toVector)
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
