// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util._

/** An immutable observation of the running capture widget, not a restore owner.
  * Low bits contain the original transport's control_state; the eight original
  * widget-local registers follow in the order below, least significant first.
  * The NASTI adapter, stream engine, vendor FSM encodings and this staging bank
  * still require independent ownership before production checkpoint admission.
  */
object CheckpointLiveWidgetSnapshot {
  val localFields = Seq("invalid" -> 1, "producedFrames" -> 64,
    "producedSnapshot" -> 64, "hostDrainedFrames" -> 64,
    "counterOverflow" -> 1, "localValid" -> 1, "localData" -> 32, "localError" -> 1)
  val localBits = localFields.map(_._2).sum
  val registers = Seq("SHADOW_COMMAND" -> 0x78, "SHADOW_INDEX" -> 0x7c,
    "SHADOW_WORD_LO" -> 0x80, "SHADOW_WORD_HI" -> 0x84, "SHADOW_STATUS" -> 0x88,
    "SHADOW_CUT_LO" -> 0x8c, "SHADOW_CUT_HI" -> 0x90, "SHADOW_WORDS" -> 0x94)

  /** Use the independently versioned source-register ABI, not a freeze request,
    * to certify that the coordinator has accepted this exact target label.
    * Physical and nonphysical transports have the same low 411-bit layout.
    */
  def acknowledgedCut(state: UInt, cycle: UInt): Bool = {
    val schema = midas.checkpoint.CheckpointTransportControlState
    schema.sources.foreach { case (name, expected) =>
      val in = getClass.getResourceAsStream("/" + name)
      require(in != null, "missing shadow source ABI resource: " + name)
      val bytes = try in.readAllBytes() finally in.close()
      val actual = java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
        .map(b => f"${b & 255}%02x").mkString
      require(actual == expected, "changed shadow coordinator source ABI: " + name)
    }
    val fields = schema.fields(1).map(f => f.path -> f).toMap
    def field(name: String): UInt = {
      val f = fields("transport.cut." + name)
      state(f.offset + f.width - 1, f.offset)
    }
    (field("state") === 2.U || field("state") === 3.U) &&
      field("error") === 0.U && field("cut_cycle") === cycle
  }
}

/** Uses only the ungated host clock. Capture samples BEFORE the accepting edge,
  * after all caller-certified owners have stopped, without holding the source.
  * Rejected requests preserve an occupied bank and its label/index/payload.
  */
class CheckpointLiveWidgetSnapshot(val bits: Int) extends Module {
  require(bits > 0)
  val words = (bits + 63) / 64
  val io = IO(new Bundle {
    val live = Input(UInt(bits.W))
    val cycle = Input(UInt(64.W))
    val cutValid = Input(Bool())
    val access = Input(Bool())
    val write = Input(Bool())
    val address = Input(UInt(8.W))
    val data = Input(UInt(32.W))
    val response = Output(UInt(32.W))
    val error = Output(Bool())
  })
  val occupied = RegInit(false.B)
  val visible = occupied && !reset.asBool
  val saved = RegInit(0.U(bits.W))
  val label = RegInit(0.U(64.W))
  val index = RegInit(0.U(32.W))
  val command = io.address === 0x78.U
  val select = io.address === 0x7c.U
  val lo = io.address === 0x80.U
  val hi = io.address === 0x84.U
  val status = io.address === 0x88.U
  val cycleLo = io.address === 0x8c.U
  val cycleHi = io.address === 0x90.U
  val count = io.address === 0x94.U
  val capture = command && io.write && io.data === 1.U
  val release = command && io.write && io.data === 2.U
  val abort = command && io.write && io.data === 4.U
  // Bound the dynamic shift before constructing its hardware: an unbounded
  // 32-bit index would infer a very large barrel shifter and could wrap.
  val offset = Cat(index(log2Ceil(words max 2) - 1, 0), 0.U(6.W))
  val word = (saved.pad(words * 64) >> offset)(63, 0)
  io.error := reset.asBool || Mux(io.write,
    Mux(command, !(capture && !occupied && io.cutValid || release && occupied || abort),
      !select || io.data >= words.U),
    !(select || status || count || (lo || hi || cycleLo || cycleHi) && visible))
  io.response := MuxCase(0.U, Seq(select -> index, status -> visible.asUInt,
    count -> words.U, (lo && visible) -> word(31, 0), (hi && visible) -> word(63, 32),
    (cycleLo && visible) -> label(31, 0), (cycleHi && visible) -> label(63, 32)))
  when(io.access && !io.error && io.write) {
    when(capture) { saved := io.live; label := io.cycle; occupied := true.B; index := 0.U }
    when(release || abort) { occupied := false.B }
    when(select) { index := io.data }
  }
}
