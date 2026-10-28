// See LICENSE for license details.
package midas.widgets

import chisel3.stage.ChiselStage
import firesim.lib.nasti.NastiParameters
import firrtl.{CircuitState, ChirrtlForm, Parser}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import java.nio.file.{Files, Paths}
import org.scalatest.flatspec.AnyFlatSpec

class CheckpointCaptureNastiStateSpec extends AnyFlatSpec {
  behavior.of("Checkpoint adapter observation coverage")

  it should "cover every original register at narrow, concrete and maximum platform widths" in {
    val root = Paths.get(sys.props("checkpoint.widget.out")).resolve("adapter-state")
    Seq((8, 1), (32, 4), (64, 64)).foreach { case (addr, id) =>
      val out = root.resolve(s"$addr-$id"); Files.createDirectories(out)
      new ChiselStage().emitVerilog(new CheckpointNastiAdapter(NastiParameters(32, addr, id)),
        Array("--target-dir", out.toString))
      val text = Files.readString(out.resolve("CheckpointNastiAdapter.fir"))
      val low = new Compiler(Forms.LowForm).execute(CircuitState(Parser.parse(text), ChirrtlForm))
      val module = low.circuit.modules.collectFirst { case m: Module if m.name == "CheckpointNastiAdapter" => m }.get
      val registers = scala.collection.mutable.Map.empty[String, BigInt]
      def visit(s: Statement): Unit = {
        s match { case r: DefRegister => registers += r.name -> r.tpe.asInstanceOf[UIntType].width.asInstanceOf[IntWidth].width
          case _ => () }
        s.foreachStmt(visit)
      }
      visit(module.body)
      assert(registers.keySet == CheckpointNastiState.fields.map(_._1).toSet,
        "unobserved or invented original adapter register")
      CheckpointNastiState.fields.foreach { case (name, width) =>
        val expected = if (name.endsWith("_addr")) addr else if (name.endsWith("_id") || name == "responseId") id else width
        assert(registers(name) == expected, s"changed original register shape: $name")
      }
      assert(CheckpointNastiState.bits == 530 && registers.size == 38)
    }
  }

  it should "reject unsupported address, ID and data widths before emitting a partial observation" in {
    Seq(NastiParameters(64,32,4), NastiParameters(32,7,4), NastiParameters(32,65,4),
      NastiParameters(32,32,0), NastiParameters(32,32,65)).foreach { np =>
      intercept[IllegalArgumentException] { new ChiselStage().emitVerilog(new CheckpointNastiAdapter(np)) }
    }
  }
}
