// See LICENSE for license details.
package midas.core

import chisel3._
import chisel3.util.HasBlackBoxInline
import firrtl.CircuitState
import firrtl.ir
import midas.checkpoint._
import midas.widgets._
import scala.collection.immutable.ListMap

/** Elaboration attachment for an independently reconstructed native network.
  * The transport stays on FPGATop's raw clock; only functional clocks inside
  * the generated network are masked. The callback must supply every original
  * non-clock port, target-cycle authority and freeze owner. No owner is inferred
  * from DMA readiness or supplied with a default acknowledgement.
  *
  * This is an opt-in attachment facility, not complete CPU capture admission.
  * In particular, external writers, resets, software owners and physical CE
  * restore still require their own complete production contracts.
  */
abstract class NativeCheckpointCaptureBinding(reference: CircuitState, manifest: String,
    plan: String, dependencies: String, owners: Seq[String], queueDepth: Int = 64)
    extends CheckpointCaptureTopBinding {
  require(!reference.circuit.modules.exists(m => Set("F1Shim", "FPGATop", "SimWrapper", "TargetBox",
    "CheckpointCaptureWidget", "CPUManagedStreamEngine").contains(m.name)),
    "native capture attachment: enclosing simulator/control harness requires in-place production mapping; cannot embed a second simulator")
  private val banks = GeneratedStateBankAccess.generatePhysicalCompanion(reference, manifest, plan, dependencies)
  val descriptors = CheckpointCaptureDescriptors.fromNativeGenerated(banks, reference, manifest, plan, dependencies)
  private val wired = PhysicalClockCutWiring.generateNative(reference, manifest, plan, dependencies, banks)
  private val files = NativeCheckpointStateAccess.emit(reference, manifest, plan, dependencies, banks, wired)
  val params = CheckpointCaptureWidgetParams(descriptors.entries.size, descriptors.manifestSha256,
    owners, queueDepth, Some(descriptors), descriptors.physicalClockGates)
  private val root = reference.circuit.modules.find(_.name == reference.circuit.main).get.asInstanceOf[ir.Module]
  private val commonClock = (org.json4s.native.JsonMethods.parse(plan) \ "common_source_clock") match {
    case org.json4s.JString(name) => name
    case _ => throw new IllegalArgumentException("native capture attachment: missing common source clock")
  }
  require(root.ports.exists(p => p.name == commonClock && p.direction == ir.Input && p.tpe == ir.ClockType),
    "native capture attachment: common source must be an original clock input")

  private def data(t: ir.Type): Data = t match {
    case ir.UIntType(ir.IntWidth(w)) if w > 0 && w.isValidInt => UInt(w.toInt.W)
    case ir.SIntType(ir.IntWidth(w)) if w > 0 && w.isValidInt => SInt(w.toInt.W)
    case ir.ClockType => Clock()
    case ir.ResetType => Reset()
    case ir.AsyncResetType => AsyncReset()
    case _ => throw new IllegalArgumentException("native capture attachment: resolved ground ports required")
  }
  private class NetworkPorts extends Record {
    val elements = ListMap((root.ports.map { p =>
      p.name -> (if (p.direction == ir.Input) Input(data(p.tpe)) else Output(data(p.tpe)))
    } ++ Seq(
      "capture_reset" -> Input(Bool()), "capture_clock_mask" -> Input(Bool()),
      "capture_read_enable" -> Input(Bool()),
      "capture_captured_ce" -> Input(UInt(descriptors.physicalClockGates.W)),
      "capture_logical_ce" -> Output(UInt(descriptors.physicalClockGates.W)),
      "capture_descriptor_entry" -> Input(UInt(32.W)), "capture_descriptor_valid" -> Output(Bool()),
      "capture_descriptor_bank" -> Output(UInt(8.W)), "capture_descriptor_slot" -> Output(UInt(16.W)),
      "capture_descriptor_width" -> Output(UInt(16.W)), "capture_descriptor_depth" -> Output(UInt(32.W)),
      "capture_request_valid" -> Input(Bool()), "capture_request_ready" -> Output(Bool()),
      "capture_bank" -> Input(UInt(8.W)), "capture_slot" -> Input(UInt(16.W)),
      "capture_address" -> Input(UInt(32.W)), "capture_response_valid" -> Output(Bool()),
      "capture_response_ready" -> Input(Bool()), "capture_response_data" -> Output(UInt(64.W)),
      "capture_response_error" -> Output(Bool()))): _*)
  }
  private class Network extends BlackBox with HasBlackBoxInline {
    override def desiredName = root.name + "CheckpointStateAccess"
    val io = IO(new NetworkPorts)
    // Emit the same validated native clock/bank network used by the descriptor
    // factory, without running the normal emitter's unrelated transformations.
    private val emitter = new SemanticCleanEmitter
    private val constrained = midas.passes.WriteXDCFile.execute(wired.state.copy(annotations = wired.state.annotations :+
      midas.targetutils.xdc.XDCPathToCircuitAnnotation(None, None)))
    private val rtl = new firrtl.stage.transforms.Compiler(
      Seq(firrtl.options.Dependency.fromTransform(emitter)), firrtl.stage.Forms.LowForm, Set(emitter))
      .execute(constrained.copy(annotations = constrained.annotations :+
        firrtl.EmitCircuitAnnotation(emitter.getClass))).getEmittedCircuit.value
    private val physicalBank = PhysicalClockStateBank.emit(reference, manifest, plan, dependencies)(1).body
    setInline(desiredName + ".sv", rtl + "\n" + physicalBank + "\n" +
      files.filter(f => f.fileSuffix.endsWith(".sv")).map(_.body).mkString("\n"))
  }

  /** Deliberately leaves all functional inputs and owner inputs uninitialized. */
  protected def connectOwnersAndFunctional(top: FPGATopImp, capture: CheckpointCaptureSideband,
      functional: Map[String, Data]): Unit

  final def connect(top: FPGATopImp, capture: CheckpointCaptureSideband): Unit = {
    require(capture.physical.nonEmpty && capture.physical.get.logicalCE.getWidth == descriptors.physicalClockGates,
      "native capture attachment: physical widget mode/count mismatch")
    val network = Module(new Network).suggestName("nativeCapture")
    val ports = network.io.elements
    def in(name: String, value: Data): Unit = ports(name) := value
    def out(name: String): UInt = ports(name).asUInt
    val physical = capture.physical.get
    in(commonClock, top.clock)
    in("capture_reset", capture.controllerReset)
    in("capture_clock_mask", physical.clockMask)
    in("capture_read_enable", physical.readEnable)
    in("capture_captured_ce", physical.capturedCE)
    physical.logicalCE := out("capture_logical_ce")
    in("capture_descriptor_entry", capture.descriptorEntry)
    capture.descriptorValid := out("capture_descriptor_valid")
    capture.descriptorBank := out("capture_descriptor_bank")
    capture.descriptorSlot := out("capture_descriptor_slot")
    capture.descriptorWidth := out("capture_descriptor_width")
    capture.descriptorDepth := out("capture_descriptor_depth")
    in("capture_request_valid", capture.bankRequestValid)
    in("capture_bank", capture.bank); in("capture_slot", capture.slot); in("capture_address", capture.address)
    capture.bankRequestReady := out("capture_request_ready")
    in("capture_response_ready", capture.bankResponseReady)
    capture.bankResponseValid := out("capture_response_valid")
    capture.bankResponseData := out("capture_response_data")
    capture.bankResponseError := out("capture_response_error")
    connectOwnersAndFunctional(top, capture, root.ports.filterNot(_.name == commonClock)
      .map(p => p.name -> ports(p.name)).toMap)
  }
}
