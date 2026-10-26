// See LICENSE for license details.
package midas.checkpoint

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import firrtl.{CircuitState, LowForm}
import firrtl.annotations.JsonProtocol
import firrtl.ir._
import midas.passes.SimulationInlineResultAnnotation
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec

/** Uses the actual registered-widget fixture emitted by the full Scala suite,
  * including the real native inline pass and its complete storage history.
  */
class NativeCheckpointWidgetPeerRoutingSpec extends AnyFlatSpec {
  import SemanticStateInventory.{canonical, sha256}
  private lazy val input = Paths.get(sys.props("checkpoint.peer.input"))
  private lazy val out = Paths.get(sys.props("checkpoint.peer.out"))
  private def read(n: String) = new String(Files.readAllBytes(input.resolve(n)), UTF_8)
  private def circuit(n: String): Circuit = {
    val working = firrtl.passes.ToWorkingIR.run(firrtl.Parser.parse(read(n)))
    firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(working))
  }
  private lazy val before = CircuitState(circuit("reserved.fir"), LowForm,
    JsonProtocol.deserialize(read("reserved-annotations.json")).filterNot(
      _.isInstanceOf[firrtl.stage.RunFirrtlTransformAnnotation]) :+
      firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.passes.InlineInstances))
  private lazy val after = CircuitState(circuit("native-inlined.fir"), LowForm,
    JsonProtocol.deserialize(read("native-inlined-annotations.json")))
  private val rejections = scala.collection.mutable.ArrayBuffer.empty[String]

  // Rehash the inline receipt after deliberate faults. The routing inspector
  // must reject the circuit semantics, not merely an old digest.
  private def rehash(s: CircuitState): CircuitState = {
    val clean = s.annotations.filterNot(_.isInstanceOf[SimulationInlineResultAnnotation])
    val previous = s.annotations.collectFirst { case a: SimulationInlineResultAnnotation => parse(a.body) }.get
    val fields = previous.asInstanceOf[JObject].obj.map {
      case ("output_sha256", _) => "output_sha256" -> JString(sha256(s.circuit.serialize))
      case ("output_annotations_sha256", _) => "output_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(clean)))
      case field => field
    }
    s.copy(annotations = clean :+ SimulationInlineResultAnnotation(canonical(JObject(fields))))
  }
  private def change(f: Statement => Statement): CircuitState = rehash(after.copy(circuit =
    after.circuit.copy(modules = after.circuit.modules.map {
      case m: Module =>
        def walk(s: Statement): Statement = f(s.mapStmt(walk))
        m.copy(body = walk(m.body))
      case m => m
    })))
  private def reject(s: CircuitState, why: String): Unit = {
    val error = intercept[IllegalArgumentException](NativeCheckpointWidgetPeerRouting.inspect(before, s))
    assert(error.getMessage.contains(why), error.getMessage)
    rejections += error.getMessage
  }
  behavior.of("Native inlined checkpoint widget peer routes")

  it should "bind all twenty actual peer routes and retain unsupported state and image guards" in {
    val result = NativeCheckpointWidgetPeerRouting.inspect(before, after)
    val doc = parse(result)
    assert((doc \ "pins").asInstanceOf[JArray].arr.size == 20)
    assert((doc \ "external_transport_state_owner") == JString("unsupported_no_state_adapter"))
    assert((doc \ "complete_fpga_capture") == JBool(false))
    intercept[IllegalArgumentException](NativeCheckpointWidgetReservation.requireBound(after))
    val error = intercept[IllegalArgumentException](SimulatorStateInventory.discover(after,
      "CheckpointPhysicalCaptureTransport"))
    assert(error.getMessage.contains("no exact state adapter"))
    Files.createDirectories(out)
    Files.write(out.resolve("inline-input-reservation.json"),
      NativeCheckpointWidgetReservation.inspect(before).getBytes(UTF_8))
    Files.write(out.resolve("inline-input-annotations.json"), JsonProtocol.serialize(before.annotations).getBytes(UTF_8))
    Files.write(out.resolve("peer-routing.json"), result.getBytes(UTF_8))
    Files.write(out.resolve("native-inlined.fir"), after.circuit.serialize.getBytes(UTF_8))
    Files.write(out.resolve("native-inlined-annotations.json"), JsonProtocol.serialize(after.annotations).getBytes(UTF_8))
    println("NATIVE_WIDGET_PEER_ROUTES pins=20 external_owner_unsupported=true")
  }

  it should "reject stale history, missing markers and changed owner or transport identity" in {
    val pins = after.annotations.collect { case p: NativeCheckpointWidgetPinAnnotation => p }
    reject(after.copy(annotations = after.annotations.filterNot(_.isInstanceOf[SimulationInlineResultAnnotation])),
      "one native inline history")
    reject(after.copy(circuit = after.circuit.copy(info = FileInfo("stale"))), "stale inline output_sha256")
    reject(rehash(after.copy(annotations = after.annotations.filterNot(_ == pins.head))), "missing, duplicate")
    reject(rehash(after.copy(annotations = after.annotations :+ pins.head)), "missing, duplicate")
    reject(rehash(after.copy(annotations = after.annotations.map {
      case a: NativeCheckpointWidgetExecutionAnnotation => a.copy(owners = a.owners.reverse)
      case a => a
    })), "changed execution/owner/CE ordering")
    reject(rehash(after.copy(circuit = after.circuit.copy(modules = after.circuit.modules.map {
      case m: ExtModule if m.defname == "CheckpointPhysicalCaptureTransport" =>
        m.copy(params = m.params.map {
          case IntParam("MANIFEST_SHA256", _) => IntParam("MANIFEST_SHA256", 1)
          case p => p
        })
      case m => m
    }))), "changed physical transport definition")
    reject(rehash(after.copy(annotations = after.annotations.map {
      case a: firrtl.transforms.BlackBoxInlineAnno if a.name == "CheckpointCaptureCut.sv" =>
        a.copy(text = a.text + "\n")
      case a => a
    })), "changed or unresolved transport sources")
    reject(rehash(after.copy(circuit = after.circuit.copy(modules = after.circuit.modules.map {
      case m: Module if m.name == after.circuit.main => m.copy(ports = m.ports.filterNot(_.name == "reset"))
      case m => m
    }))), "missing raw host reset")
  }

  it should "reject coherently rehashed swaps, CE bit permutations, duplicate drivers, loops and gated clocks" in {
    val transport = (parse(NativeCheckpointWidgetPeerRouting.inspect(before, after)) \ "transport")
      .asInstanceOf[JString].s.split('.').last
    val pins = after.annotations.collect { case p: NativeCheckpointWidgetPinAnnotation => p.role -> p.target.ref }.toMap
    reject(change {
      case c: Connect if c.loc.serialize == transport + ".bank_response_error" =>
        c.copy(expr = firrtl.WRef(pins("response_valid"), UIntType(IntWidth(1))))
      case s => s
    }, "non-alias or wrong endpoint")
    reject(change {
      case c: Connect if c.loc.serialize == transport + ".ce" =>
        c.copy(expr = DoPrim(firrtl.PrimOps.Bits, Seq(c.expr), Seq(0, 0), UIntType(IntWidth(1))))
      case s => s
    }, "non-alias or wrong endpoint")
    val clockConnection = after.circuit.modules.collect { case m: Module => m }.flatMap { m =>
      val rows = scala.collection.mutable.ArrayBuffer.empty[Connect]
      def visit(s: Statement): Unit = { s match {
        case c: Connect if c.loc.serialize == transport + ".clock" => rows += c
        case _ => ()
      }; s.foreachStmt(visit) }
      visit(m.body); rows
    }.head
    reject(change {
      case c: Connect if c.loc.serialize == transport + ".clock" => Block(Seq(c, c))
      case s => s
    }, "missing or multiple drivers")
    reject(change {
      case c: Connect if c.loc.serialize == transport + ".clock" => c.copy(expr = c.loc)
      case s => s
    }, "alias cycle")
    reject(change {
      case c: Connect if c == clockConnection =>
        c.copy(expr = firrtl.WRef("reset", UIntType(IntWidth(1))))
      case s => s
    }, "missing or multiple drivers")
    reject(change {
      case n: DefNode if n.name == pins("descriptor_valid") => n.copy(value = UIntLiteral(1, IntWidth(1)))
      case s => s
    }, "changed peer placeholder")
    Files.write(out.resolve("negative-tests.json"),
      canonical(JObject(List("rejections" -> JArray(rejections.map(JString(_)).toList),
        "physical_fpga_capture" -> JBool(false)))).getBytes(UTF_8))
  }
}
