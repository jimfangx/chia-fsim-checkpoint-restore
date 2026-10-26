// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm}
import firrtl.ir._
import org.json4s._

/** Exact observation schema for the physical capture transport's sequential
  * registers. This is NOT an inventory adapter: these raw-clocked controllers
  * continue to change while the target is stopped. An atomic capture/restore
  * protocol, stream/host ownership and vendor CE owners remain mandatory.
  */
object CheckpointTransportControlState {
  import SemanticStateInventory.{canonical, sha256}
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "checkpoint transport control state: " + why)
  // A schema revision must audit changed RTL, not silently bless new resources.
  val sources: Map[String, String] = Map(
    "CheckpointCaptureCut.sv" -> "0b7f712c7f3fd72f6c2cdf25a40c5a1d2165ae51430b38b0dff4fe1334f97fa6",
    "CheckpointCaptureTransport.sv" -> "1e48ab970687d7bd503f539cb84970bbed60210380ff4d5add30c9a97b391fd8",
    "CheckpointPhysicalClockCut.sv" -> "3231634af06e694716d7858ee79ccd50185df64d19ddf54756eafb54a3f928c3",
    "CheckpointPhysicalCaptureTransport.sv" -> "b36cf58377a48e40e455d73602dcb0999a7146b15f3a82167286dc6ef91d434d")

  case class Field(path: String, width: Int, offset: Int, edge: String, reset: BigInt)
  def fields(gates: Int): Seq[Field] = {
    check(gates > 0 && gates <= 65535, "invalid physical gate count")
    val transport = Seq("state" -> 4, "saved_bank" -> 8, "saved_slot" -> 16,
      "saved_width" -> 16, "saved_depth" -> 32, "saved_data" -> 64, "words" -> 64,
      "stream_crc" -> 32, "outstanding" -> 1, "transport_error" -> 4,
      "mmio_response_valid" -> 1, "mmio_rdata" -> 32, "mmio_response_error" -> 1,
      "descriptor_entry" -> 32, "address" -> 32).map { case (n, w) => ("transport." + n, w) }
    val cut = Seq("state" -> 3, "cut_cycle" -> 64, "error" -> 5).map {
      case (n, w) => ("transport.cut." + n, w)
    }
    val physical = Seq("freeze_low" -> 1, "sampled" -> 1, "release_pending" -> 1,
      "captured_ce" -> gates, "cut_cycle" -> 64, "error" -> 3).map {
      case (n, w) => ("physical." + n, w)
    }
    var offset = 0
    (transport ++ cut ++ physical).map { case (path, width) =>
      val field = Field(path, width, offset,
        if (path == "physical.freeze_low") "negedge clock" else "posedge clock",
        if (path == "transport.stream_crc") BigInt("ffffffff", 16) else BigInt(0))
      offset += width; field
    }
  }

  /** Banked combinational observation. No latch, hidden queue or sampling edge
    * is introduced. Consumers MUST sample the entire live vector coherently;
    * successive words during DMA/MMIO activity are not an atomic checkpoint.
    */
  def readout(gates: Int): String = {
    val bits = fields(gates).map(_.width).sum
    s"""module CheckpointTransportControlReadout(
  input wire [${bits - 1}:0] control_state,
  input wire [31:0] word_index,
  output wire word_valid,
  output wire [63:0] word_data
);
  assign word_valid = word_index < ${(bits + 63) / 64};
  assign word_data = word_valid ? (control_state >> (word_index * 64)) : 64'b0;
endmodule
"""
  }

  def inspect(state: CircuitState): String = {
    check(state.form == LowForm, "resolved LowForm required")
    val transports = GeneratedExternalOwners.instances(state, state.circuit.main).collect {
      case (p, m: ExtModule) if m.defname == "CheckpointPhysicalCaptureTransport" => p -> m
    }
    check(transports.size == 1, "one concrete physical transport required")
    val (path, module) = transports.head
    val params = module.params.collect { case IntParam(n, v) => n -> v }.toMap
    check(module.params.size == 6 && params.size == 6 && params.keySet ==
      Set("GATES", "OWNERS", "ENTRIES", "EXTERNAL_CLOCK_NETWORK", "RESTORE_ENABLE", "MANIFEST_SHA256"),
      "unknown parameters")
    check(params("GATES").isValidInt, "invalid physical gate count")
    val gates = params("GATES").toInt
    val layout = fields(gates)
    val bits = layout.map(_.width).sum
    check(params("OWNERS") > 0 && params("OWNERS") <= 65535 &&
      params("ENTRIES") > 0 && params("ENTRIES") <= 65535 &&
      Set(BigInt(0), BigInt(1))(params("EXTERNAL_CLOCK_NETWORK")) &&
      params("RESTORE_ENABLE") == 0 &&
      params("MANIFEST_SHA256") >= 0 && params("MANIFEST_SHA256").bitLength <= 256, "invalid specialization")
    check(module.ports.count(_.name == "control_state") == 1 && module.ports.exists(p =>
      p.name == "control_state" && p.direction == Output && p.tpe == UIntType(IntWidth(bits))), "changed state ABI")
    val inline = state.annotations.collect {
      case a: firrtl.transforms.BlackBoxInlineAnno if a.target.name == module.name => a
    }
    check(inline.size == sources.size && inline.map(_.name).toSet == sources.keySet &&
      inline.forall(a => a.target.circuit.name == state.circuit.main && sha256(a.text) == sources(a.name)) &&
      !state.annotations.exists {
        case a: firrtl.transforms.BlackBoxPathAnno => a.target.name == module.name
        case a: firrtl.transforms.BlackBoxResourceAnno => a.target.name == module.name
        case _ => false
      }, "changed, unresolved or duplicated state implementation")
    val rows = layout.map(f => JObject(List("hierarchy" -> JString(path + "." + f.path),
      "relative_path" -> JString(f.path), "width" -> JInt(f.width), "offset" -> JInt(f.offset),
      "clock" -> JString(f.edge), "reset_value" -> JInt(f.reset))))
    canonical(JObject(List("schema_version" -> JInt(1),
      "scope" -> JString("live_transport_register_observation_only"), "transport" -> JString(path),
      "reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "annotations_sha256" -> JString(sha256(firrtl.annotations.JsonProtocol.serialize(state.annotations))),
      "definition_sha256" -> JString(sha256(module.serialize)),
      "sources" -> JObject(sources.toList.map { case (n, h) => n -> JString(h) }),
      "gates" -> JInt(gates), "bits" -> JInt(bits), "words" -> JInt((bits + 63) / 64),
      "state" -> JArray(rows.toList), "bank_sha256" -> JString(sha256(readout(gates))),
      "sampling_contract" -> JString("live_vector_not_an_atomic_multiword_snapshot"),
      "external_transport_state_owner" -> JString("unsupported_no_atomic_capture_restore_adapter"),
      "includes_vendor_gate_state" -> JBool(false), "includes_stream_host_state" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
  }
}
