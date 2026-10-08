// See LICENSE for license details.
package firesim.midasexamples

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firesim.lib.bridges.{PeekPokeBridge, RationalClockBridge}
import midas.checkpoint.{BankedStateAccess, SemanticBankedAccessAnnotation, SemanticStateInventory, StateAccessEntry}
import org.chipsalliance.cde.config.Parameters

/** Explicit small-DUT binding. The verifier requires exact manifest membership
  * and metadata; unsupported or newly added state cannot silently disappear.
  * Compiler-generated binding/mapping is follow-on work.
  */
object InventoryBankedBinding {
  val root = "InventoryBankedFixture.dut"
  val locals = Seq("counter" -> 9, "fsm" -> 2, "bank_0" -> 9, "bank_1" -> 9,
    "bank_2" -> 9, "qValid" -> 1, "qAddr" -> 3, "busy" -> 1, "delay" -> 2,
    "pendingData" -> 9, "responseValid" -> 1, "responseData" -> 9)
  val bindings = (locals.zipWithIndex.map { case ((name, width), slot) =>
    (name, "register", 0, slot, width, 1)
  } ++ Seq(("a.saved", "register", 1, 0, 9, 1), ("b.saved", "register", 2, 0, 9, 1),
    ("sram", "memory", 3, 0, 9, 8))).sortBy { case (name, kind, _, _, _, _) =>
    SemanticStateInventory.stateID(s"$root.$name", kind)
  }
  val entries = bindings.map { case (name, kind, bank, slot, width, depth) =>
    StateAccessEntry(SemanticStateInventory.stateID(s"$root.$name", kind), bank, slot, width, depth)
  }
}

class InventoryBankedFixture(implicit p: Parameters) extends RawModule {
  annotate(new ChiselAnnotation {
    def toFirrtl = SemanticBankedAccessAnnotation(InventoryBankedBinding.root, InventoryBankedBinding.entries)
  })
  val clock = RationalClockBridge().io.clocks.head
  val reset = WireInit(false.B)
  withClockAndReset(clock, reset) {
    val dut = Module(new InventoryFixtureDUT(checkpointAccess = true))
    val access = Module(new BankedStateAccess(InventoryBankedBinding.entries))
    // Initialize the controller on the first edge only. Functional resets are
    // ordinary semantic edges; they never reset the checkpoint cycle epoch.
    val controllerReset = Wire(Input(Bool()))
    access.reset := controllerReset
    dut.io.checkpoint.get.freeze := access.io.freeze
    dut.io.checkpoint.get.bank <> access.io.bank
    // Fault injection is at the client interface, for explicit incomplete-state
    // failure testing. It never supplies replacement semantic values.
    val fault = Wire(Input(Bool()))
    access.io.bank.error := dut.io.checkpoint.get.bank.error || fault
    PeekPokeBridge(clock, reset,
      (dut.io.elements.toSeq.filterNot(_._1 == "checkpoint").map { case (name, data) => s"io_$name" -> data } ++
        Seq("cp_start" -> access.io.start, "cp_command" -> access.io.command,
          "cp_clearError" -> access.io.clearError, "cp_ready" -> access.io.startReady,
          "cp_freeze" -> access.io.freeze, "cp_done" -> access.io.done,
          "cp_error" -> access.io.error, "cp_cycle" -> access.io.cycle,
          "cp_valid" -> access.io.stream.valid, "cp_take" -> access.io.stream.ready,
          "cp_entry" -> access.io.stream.bits.entry, "cp_address" -> access.io.stream.bits.address,
          "cp_width" -> access.io.stream.bits.bitWidth, "cp_data" -> access.io.stream.bits.data,
          "cp_last" -> access.io.stream.bits.last, "cp_beatError" -> access.io.stream.bits.error,
          "cp_fault" -> fault, "cp_reset" -> controllerReset)): _*)
  }
}
