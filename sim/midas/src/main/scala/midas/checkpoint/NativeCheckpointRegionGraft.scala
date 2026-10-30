// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, WDefInstance, WRef, WSubField}
import firrtl.ir._
import midas.stage.GoldenGateOutputFileAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** One raw-clock transport and one mask/sample interface for disjoint native
  * banks installed in their existing host. This is a readout graft, not an
  * acknowledgement of complete simulator/host/functional-memory ownership.
  */
object NativeCheckpointRegionGraft {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(state: CircuitState, files: Seq[GoldenGateOutputFileAnnotation], binding: String)
  private def port(p: Port): String = {
    val (signed, width) = p.tpe match {
      case UIntType(IntWidth(w)) if w > 0 => ("", w)
      case SIntType(IntWidth(w)) if w > 0 => ("signed ", w)
      case ClockType | ResetType | AsyncResetType => ("", BigInt(1))
      case _ => throw new IllegalArgumentException("native region graft: unresolved or nonground port")
    }
    s"  ${if (p.direction == Input) "input" else "output"} wire $signed${if (width == 1) "" else s"[${width-1}:0] "}${p.name}"
  }
  def generate(source: NativeCheckpointRegions.Result, stageNativeState: Boolean = false): Result = {
    NativeCheckpointRegions.validate(source)
    require(!stageNativeState || source.regions.isEmpty,
      "native region graft: staging currently supports shell-only endpoints; physical CE/common-cut owners remain unsupported")
    val state = source.state
    val original = source.host.circuit.modules.find(_.name == source.host.circuit.main).get.asInstanceOf[Module]
    val root = state.circuit.modules.find(_.name == state.circuit.main).get.asInstanceOf[Module]
    val access = root.ports.drop(original.ports.size)
    require(root.ports.take(original.ports.size) == original.ports && access.nonEmpty &&
      access.forall(p => p.name.startsWith(NativeCheckpointInPlace.prefix) ||
        p.name.startsWith(NativeCheckpointShellStateAccess.prefix)), "native region graft: changed interface")
    val JString(clock) = parse(source.binding) \ "raw_clock"
    val clockPort = original.ports.find(p => p.name == clock && p.direction == Input && p.tpe == ClockType).get
    val name = root.name + "CheckpointRegionReadoutAdapter"
    val instance = "checkpoint_region_readout_adapter"
    // Synchronous native readers introduce live state that does not belong to
    // the original descriptor table. Keep it observable in immutable shell/
    // field order; freeze alone does not stabilize the borrowed RAM pipeline.
    // A downstream atomic staging owner is still required before capture.
    var nativeBits = BigInt(0)
    val observations = source.shells.zipWithIndex.flatMap { case (shell, index) =>
      (parse(shell.banks.binding) \ "native_state_observation") match {
        case JNothing => Nil
        case observation: JObject =>
          val JInt(bits) = observation \ "bits"
          require(bits > 0 && bits.isValidInt, "native region graft: invalid native observation width")
          val localPort = shell.sidebandPrefix + NativeSynchronousStateObservation.port
          require(access.exists(p => p.name == localPort && p.direction == Output &&
            p.tpe == UIntType(IntWidth(bits))), "native region graft: missing native observation port")
          val JString(execution) = parse(shell.mapping) \ "selected_execution"
          val row = JObject(List("region" -> JInt(source.regions.size + index),
            "selected_execution" -> JString(execution), "port" -> JString(localPort),
            "offset" -> JInt(nativeBits), "bits" -> JInt(bits),
            "banks_binding_sha256" -> JString(sha256(shell.banks.binding)), "observation" -> observation))
          nativeBits += bits
          Seq(row)
        case _ => throw new IllegalArgumentException("native region graft: invalid native observation")
      }
    }
    require(nativeBits.isValidInt, "native region graft: native observation exceeds FIRRTL width")
    val nativePorts = if (nativeBits == 0) Nil else
      Seq(Port(NoInfo, "capture_native_state", Output, UIntType(IntWidth(nativeBits))))
    require(!stageNativeState || nativeBits > 0,
      "native region graft: atomic staging requires a native observation owner")
    def snapshotPort(n: String, d: Direction, bits: BigInt = 1): Port =
      Port(NoInfo, "capture_snapshot_" + n, d, UIntType(IntWidth(bits)))
    val snapshotPorts = if (!stageNativeState) Nil else Seq(
      snapshotPort("request", Input), snapshotPort("release", Input), snapshotPort("owners_present", Input),
      snapshotPort("target_cycle", Input, 64), snapshotPort("word_address", Input, 32),
      snapshotPort("ready", Output), snapshotPort("valid", Output), snapshotPort("failed", Output),
      snapshotPort("borrow_enable", Output), snapshotPort("word_error", Output),
      snapshotPort("word_data", Output, 64), snapshotPort("cut_cycle", Output, 64),
      snapshotPort("error", Output, 6), snapshotPort("state", Output, nativeBits),
      snapshotPort("control_state", Output, nativeBits + 71))
    val ports = NativeCheckpointInPlaceGraft.capturePorts(source.descriptors.physicalClockGates) ++ nativePorts ++ snapshotPorts
    val declarations = scala.collection.mutable.ArrayBuffer.empty[String]
    def visit(s: Statement): Unit = { s match { case d: IsDeclaration => declarations += d.name; case _ => () }; s.foreachStmt(visit) }
    visit(root.body)
    require(!(declarations ++ root.ports.map(_.name)).exists(n => n == instance || n.startsWith("capture_")) &&
      !state.circuit.modules.exists(m => Set(name, "CheckpointRegionStateRouter", "CheckpointPhysicalStateRouter")(m.name)),
      "native region graft: reserved identifier collision")
    val adapterPorts = Seq(clockPort) ++ access.map(p => p.copy(direction = if (p.direction == Input) Output else Input))
    val ext = ExtModule(NoInfo, name, adapterPorts ++ ports, name, Nil)
    val connections = ext.ports.map { p =>
      val pin = WSubField(WRef(instance), p.name, p.tpe); val wire = WRef(p.name, p.tpe)
      if (p.direction == Input) Connect(NoInfo, pin, wire) else Connect(NoInfo, wire, pin)
    }
    val next = root.copy(ports = original.ports ++ ports,
      body = Block(access.map(p => DefWire(NoInfo, p.name, p.tpe)) ++
        Seq(WDefInstance(NoInfo, instance, name, firrtl.Utils.module_type(ext)), root.body) ++ connections))
    val circuit = state.circuit.copy(modules = state.circuit.modules.map(m => if (m.name == root.name) next else m) :+ ext)
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val files = scala.collection.mutable.ArrayBuffer.empty[GoldenGateOutputFileAnnotation]
    val nativeEndpoints = source.regions.zip(source.routing.regions).zipWithIndex.map { case ((r, allocation), i) =>
      val prefix = NativeCheckpointRegions.prefix(i)
      val localPorts = Seq(clockPort) ++ adapterPorts.filter(_.name.startsWith(prefix))
      val localName = name + s"Region$i"
      val ce = PhysicalClockStateBank.emit(r.reference, r.manifest, r.plan, r.dependencies)
      val JString(ceModule) = parse(ce.head.body) \ "endpoint_module"
      require(!state.circuit.modules.exists(m => m.name == localName || m.name == ceModule),
        "native region graft: endpoint definition collision")
      files += GoldenGateOutputFileAnnotation(NativeCheckpointStateAccess.render(original, localName, clock,
        allocation.ceCount, r.native.identity.entries.filterNot(_.bank == 255), ceModule, prefix, Some(localPorts)),
        s".native-region-graft-$i.sv")
      files += GoldenGateOutputFileAnnotation(ce(1).body, s".native-region-graft-$i-ce.sv")
      val ceSlice = s"[${allocation.ceBase} +: ${allocation.ceCount}]"
      val pass = localPorts.map(p => s".${p.name}(${p.name})")
      val control = Seq("reset", "clock_mask", "read_enable").map(n =>
        s".capture_$n(${if (stageNativeState && n == "read_enable") "staged_read_enable" else "capture_" + n})") ++ Seq(
        s".capture_captured_ce(capture_captured_ce$ceSlice)", s".capture_logical_ce(capture_logical_ce$ceSlice)",
        ".capture_descriptor_entry(32'd0)") ++ Seq("valid", "bank", "slot", "width", "depth").map(n => s".capture_descriptor_$n()")
      val transaction = Seq("request_valid", "request_ready", "response_valid", "response_ready", "response_error")
        .map(n => s".capture_$n(region_${n}[$i])") ++ Seq("bank", "slot", "address").map(n => s".capture_$n(region_$n)") ++
        Seq(s".capture_response_data(region_response_data[${i*64} +: 64])")
      s"  $localName region$i(\n    ${(pass ++ control ++ transaction).mkString(",\n    ")});"
    }
    val shellEndpoints = source.shells.zipWithIndex.map { case (shell, index) =>
      val i = source.regions.size + index
      val allocation = source.routing.regions(i)
      require(allocation.ceCount == 0, "native region graft: shell cannot claim physical CE state")
      val prefix = shell.sidebandPrefix
      val local = adapterPorts.filter(_.name.startsWith(prefix))
      val signals = Seq("ckpt_clock" -> clock, "ckpt_reset" -> "capture_reset", "ckpt_freeze" -> "capture_clock_mask",
        "ckpt_bank" -> "region_bank", "ckpt_slot" -> "region_slot", "ckpt_address" -> "region_address",
        "ckpt_req_valid" -> s"region_request_valid[$i]", "ckpt_req_ready" -> s"region_request_ready[$i]",
        "ckpt_resp_valid" -> s"region_response_valid[$i]", "ckpt_resp_ready" -> s"region_response_ready[$i]",
        "ckpt_resp_data" -> s"region_response_data[${i*64} +: 64]", "ckpt_resp_error" -> s"region_response_error[$i]")
      val observed = observations.filter(r => (r \ "region") == JInt(i)).map(r =>
        (r \ "port").asInstanceOf[JString].s)
      require(local.map(_.name).toSet == (signals.map(x => prefix + x._1) ++
        Seq(prefix + "ckpt_match", prefix + "ckpt_data") ++ observed).toSet,
        "native region graft: unknown shell interface")
      signals.map { case (pin, wire) =>
        val p = local.find(_.name == prefix + pin).get
        if (p.direction == Input) s"  assign $wire = ${p.name};" else s"  assign ${p.name} = $wire;"
      }.mkString("\n")
    }
    val endpoints = (nativeEndpoints ++ shellEndpoints).mkString("\n")
    val nativeRTL = observations.map { row =>
      val JInt(offset) = row \ "offset"
      val JInt(bits) = row \ "bits"
      val JString(pin) = row \ "port"
      s"  assign capture_native_state[$offset +: $bits] = $pin;"
    }.mkString("\n")
    val cases = source.descriptors.entries.zipWithIndex.map { case (e, i) =>
      s"      32'd$i: begin capture_descriptor_valid=1; capture_descriptor_bank=8'd${e.bank}; " +
        s"capture_descriptor_slot=16'd${e.slot}; capture_descriptor_width=16'd${e.width}; capture_descriptor_depth=32'd${e.depth}; end"
    }.mkString("\n")
    val n = source.routing.regions.size
    // Idle includes every endpoint, not only the currently addressed region.
    // An outstanding response must stay pending on a lost cut; neither router
    // nor endpoint may acknowledge it after permission is revoked. Abort then
    // requires a common reset, not an invented completion or a new snapshot.
    val stagingRTL = if (!stageNativeState) "" else s"""
  wire raw_request_ready, raw_response_valid, raw_response_error;
  wire [63:0] raw_response_data;
  wire staged_permission = capture_snapshot_borrow_enable;
  wire staged_read_enable = capture_read_enable && staged_permission;
  wire all_readers_idle = raw_request_ready && (&region_request_ready) && !(|region_response_valid);
  assign capture_request_ready = raw_request_ready && staged_permission;
  assign capture_response_valid = raw_response_valid && staged_permission;
  assign capture_response_data = staged_permission ? raw_response_data : 64'b0;
  assign capture_response_error = raw_response_error && staged_permission;
  CheckpointNativeStateSnapshot #(.BITS($nativeBits)) native_snapshot(
    .clock($clock), .reset(capture_reset), .restore_hold(1'b0), .restore_load(1'b0),
    .restore_state(${nativeBits + 71}'d0), .restore_ready(),
    .request(capture_snapshot_request), .release_snapshot(capture_snapshot_release),
    .owners_present(capture_snapshot_owners_present),
    .functional_frozen(capture_clock_mask && capture_read_enable), .reader_idle(all_readers_idle),
    .borrow_attempt(capture_request_valid), .target_cycle(capture_snapshot_target_cycle),
    .live_state(capture_native_state), .word_address(capture_snapshot_word_address),
    .request_ready(capture_snapshot_ready), .valid(capture_snapshot_valid),
    .borrow_enable(capture_snapshot_borrow_enable), .failed(capture_snapshot_failed),
    .word_data(capture_snapshot_word_data), .word_error(capture_snapshot_word_error),
    .snapshot_state(capture_snapshot_state), .cut_cycle(capture_snapshot_cut_cycle),
    .error(capture_snapshot_error), .control_state(capture_snapshot_control_state));
"""
    def routerSignal(n: String): String = if (!stageNativeState) "capture_" + n else n match {
      case "request_valid" | "response_ready" => "capture_" + n + " && staged_permission"
      case _ => "raw_" + n
    }
    val declarationsRTL = (adapterPorts ++ ports).map { p =>
      val text = port(p)
      if (p.name.startsWith("capture_descriptor_") && p.direction == Output) text.replace("output wire", "output reg") else text
    }.mkString(",\n")
    val rtl = s"""// Generated shared raw readout. All-owner freeze/restore remains unsupported.
module $name (
$declarationsRTL
);
  always @* begin
    capture_descriptor_valid=0; capture_descriptor_bank=0; capture_descriptor_slot=0;
    capture_descriptor_width=0; capture_descriptor_depth=0;
    case(capture_descriptor_entry)
$cases
      default: begin end
    endcase
  end
  wire [${n-1}:0] region_request_valid, region_request_ready, region_response_valid, region_response_ready, region_response_error;
  wire [${n*64-1}:0] region_response_data;
  wire [7:0] region_bank;
  wire [15:0] region_slot;
  wire [31:0] region_address;
$stagingRTL
  CheckpointRegionStateRouter #(${source.routing.parameters}) router(
    .clock($clock), .reset(capture_reset),
    .request_valid(${routerSignal("request_valid")}), .request_ready(${routerSignal("request_ready")}),
    .bank(capture_bank), .slot(capture_slot), .address(capture_address),
    .response_valid(${routerSignal("response_valid")}), .response_ready(${routerSignal("response_ready")}),
    .response_data(${routerSignal("response_data")}), .response_error(${routerSignal("response_error")}),
    .region_request_valid(region_request_valid), .region_request_ready(region_request_ready),
    .region_bank(region_bank), .region_slot(region_slot), .region_address(region_address),
    .region_response_valid(region_response_valid), .region_response_ready(region_response_ready),
    .region_response_data(region_response_data), .region_response_error(region_response_error));
$endpoints
$nativeRTL
endmodule
"""
    files += GoldenGateOutputFileAnnotation(rtl, ".native-region-graft.sv")
    def resource(n: String): String = { val s = scala.io.Source.fromResource(n); try s.mkString finally s.close() }
    files += GoldenGateOutputFileAnnotation(resource("CheckpointRegionStateRouter.sv"), ".native-region-graft-router.sv")
    files += GoldenGateOutputFileAnnotation(resource("CheckpointPhysicalStateRouter.sv"), ".native-region-graft-physical-router.sv")
    if (stageNativeState) files += GoldenGateOutputFileAnnotation(resource("CheckpointNativeStateSnapshot.sv"),
      ".native-region-graft-snapshot.sv")
    val nativeBinding = if (observations.isEmpty) Nil else List("native_state_observation" -> JObject(List(
      "version" -> JInt(1), "port" -> JString("capture_native_state"), "bits" -> JInt(nativeBits),
      "bit_order" -> JString("shell_order_lsb0"), "regions" -> JArray(observations.toList),
      "sampling_contract" -> JString("atomically_stage_entire_vector_before_any_borrow_or_bank_request"),
      "stable_while_functional_freeze" -> JBool(false), "atomic_snapshot_supported" -> JBool(false),
      "complete_capture_owner" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    val snapshotBinding = if (!stageNativeState) Nil else List("native_state_snapshot" -> JObject(List(
      "version" -> JInt(1), "module" -> JString("CheckpointNativeStateSnapshot"),
      "instance" -> JString(instance + ".native_snapshot"), "bits" -> JInt(nativeBits),
      "words" -> JInt((nativeBits + 63) / 64), "control_bits" -> JInt(nativeBits + 71),
      "control_layout_lsb0" -> JArray(List(JString("payload"), JString("occupied"), JString("cut_cycle"), JString("error"))),
      "sampling" -> JString("one_common_raw_positive_edge_preedge_values_before_any_bank_request"),
      "idle" -> JString("router_ready_and_every_endpoint_ready_and_no_endpoint_response"),
      "bank_requests_and_responses_gated" -> JBool(true), "restore_supported" -> JBool(false),
      "global_freeze_owner_bound" -> JBool(false), "owner" -> JString("capture_only_native_staging_control"))))
    val binding = canonical(JObject(List(
      "graft_version" -> JInt(1), "scope" -> JString("existing_native_regions_shared_readout_only"),
      "root" -> JString(root.name), "adapter_module" -> JString(name), "adapter_instance" -> JString(instance),
      "raw_clock" -> JString(clock), "regions_binding_sha256" -> JString(sha256(source.binding)),
      "grafted_sha256" -> JString(sha256(flowed.serialize)),
      "descriptor_manifest_sha256" -> JString(source.descriptors.manifestSha256),
      "files" -> JObject(files.map(f => f.fileSuffix -> JString(sha256(f.body))).toList),
      "common_mask_and_sample_interface" -> JBool(true), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false)) ++ nativeBinding ++ snapshotBinding))
    Result(state.copy(circuit = flowed), files.toSeq :+
      GoldenGateOutputFileAnnotation(binding, ".native-region-graft.json"), binding)
  }
}
