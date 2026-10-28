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
  def generate(source: NativeCheckpointRegions.Result): Result = {
    NativeCheckpointRegions.validate(source)
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
    val ports = NativeCheckpointInPlaceGraft.capturePorts(source.descriptors.physicalClockGates)
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
      val control = Seq("reset", "clock_mask", "read_enable").map(n => s".capture_$n(capture_$n)") ++ Seq(
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
      require(local.map(_.name).toSet == (signals.map(x => prefix + x._1) ++
        Seq(prefix + "ckpt_match", prefix + "ckpt_data")).toSet, "native region graft: unknown shell interface")
      signals.map { case (pin, wire) =>
        val p = local.find(_.name == prefix + pin).get
        if (p.direction == Input) s"  assign $wire = ${p.name};" else s"  assign ${p.name} = $wire;"
      }.mkString("\n")
    }
    val endpoints = (nativeEndpoints ++ shellEndpoints).mkString("\n")
    val cases = source.descriptors.entries.zipWithIndex.map { case (e, i) =>
      s"      32'd$i: begin capture_descriptor_valid=1; capture_descriptor_bank=8'd${e.bank}; " +
        s"capture_descriptor_slot=16'd${e.slot}; capture_descriptor_width=16'd${e.width}; capture_descriptor_depth=32'd${e.depth}; end"
    }.mkString("\n")
    val n = source.routing.regions.size
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
  CheckpointRegionStateRouter #(${source.routing.parameters}) router(
    .clock($clock), .reset(capture_reset),
    .request_valid(capture_request_valid), .request_ready(capture_request_ready),
    .bank(capture_bank), .slot(capture_slot), .address(capture_address),
    .response_valid(capture_response_valid), .response_ready(capture_response_ready),
    .response_data(capture_response_data), .response_error(capture_response_error),
    .region_request_valid(region_request_valid), .region_request_ready(region_request_ready),
    .region_bank(region_bank), .region_slot(region_slot), .region_address(region_address),
    .region_response_valid(region_response_valid), .region_response_ready(region_response_ready),
    .region_response_data(region_response_data), .region_response_error(region_response_error));
$endpoints
endmodule
"""
    files += GoldenGateOutputFileAnnotation(rtl, ".native-region-graft.sv")
    def resource(n: String): String = { val s = scala.io.Source.fromResource(n); try s.mkString finally s.close() }
    files += GoldenGateOutputFileAnnotation(resource("CheckpointRegionStateRouter.sv"), ".native-region-graft-router.sv")
    files += GoldenGateOutputFileAnnotation(resource("CheckpointPhysicalStateRouter.sv"), ".native-region-graft-physical-router.sv")
    val binding = canonical(JObject(List(
      "graft_version" -> JInt(1), "scope" -> JString("existing_native_regions_shared_readout_only"),
      "root" -> JString(root.name), "adapter_module" -> JString(name), "adapter_instance" -> JString(instance),
      "raw_clock" -> JString(clock), "regions_binding_sha256" -> JString(sha256(source.binding)),
      "grafted_sha256" -> JString(sha256(flowed.serialize)),
      "descriptor_manifest_sha256" -> JString(source.descriptors.manifestSha256),
      "files" -> JObject(files.map(f => f.fileSuffix -> JString(sha256(f.body))).toList),
      "common_mask_and_sample_interface" -> JBool(true), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
    Result(state.copy(circuit = flowed), files.toSeq :+
      GoldenGateOutputFileAnnotation(binding, ".native-region-graft.json"), binding)
  }
}
