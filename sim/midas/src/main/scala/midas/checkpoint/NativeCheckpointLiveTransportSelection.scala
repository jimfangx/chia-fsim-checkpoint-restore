// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.{IsModule, InstanceTarget, ModuleTarget, ReferenceTarget, Target}
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Select live local registers by the actual widget's compiler inline history.
  * Names/prefixes alone cannot establish transport ownership. This selection
  * keeps the original owner running, but supplies NO atomic capture or restore.
  */
object NativeCheckpointLiveTransportSelection {
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native live transport selection: " + why)

  def registers(before: CircuitState, after: CircuitState, target: IsModule): Set[String] = {
    NativeCheckpointWidgetPeerRouting.inspect(before, after)
    val JString(widget) = parse(NativeCheckpointWidgetReservation.inspect(before)) \ "widget"
    val history = after.annotations.collect { case a: midas.passes.SimulationInlineResultAnnotation => a }
    check(history.size == 1, "one actual inline storage history required")
    val live = GeneratedExternalOwners.instances(after, after.circuit.main)
    val selected = target match {
      case t: ModuleTarget =>
        check(t.circuit == after.circuit.main, "foreign module selection")
        val paths = live.filter(_._2.name == t.module)
        check(paths.size == 1, "module selection requires exactly one concrete execution")
        paths.head
      case t: InstanceTarget =>
        check(t.circuit == after.circuit.main && t.module == after.circuit.main, "unrooted instance selection")
        val path = (Seq(t.module) ++ t.asPath.map(_._1.value)).mkString(".")
        live.find { case (p, m) => p == path && m.name == t.ofModule }.getOrElse(
          throw new IllegalArgumentException("native live transport selection: stale execution"))
      case _ => throw new IllegalArgumentException("native live transport selection: unsupported target")
    }
    val module = selected._2 match {
      case m: Module => m
      case _ => throw new IllegalArgumentException("native live transport selection: concrete module required")
    }
    def hierarchy(value: JValue): String = value match {
      case JString(text) => Target.deserialize(text) match {
        case t: ReferenceTarget =>
          check(t.circuit == after.circuit.main && t.module == after.circuit.main && t.component.isEmpty,
            "unrooted inline storage owner")
          (Seq(t.module) ++ t.path.map(_._1.value) :+ t.ref).mkString(".")
        case _ => throw new IllegalArgumentException("native live transport selection: invalid storage reference")
      }
      case _ => throw new IllegalArgumentException("native live transport selection: missing storage reference")
    }
    val rows = (midas.passes.ResolveSimulationInline.context(history.head) \ "storage").asInstanceOf[JArray].arr
      .filter(r => hierarchy(r \ "from").startsWith(widget + "."))
    val local = rows.map(r => hierarchy(r \ "to")).filter { path =>
      path.startsWith(selected._1 + ".") && path.stripPrefix(selected._1 + ".").indexOf('.') < 0
    }
    check(local.distinct.size == local.size, "duplicate live storage owner")
    val declarations = scala.collection.mutable.ArrayBuffer.empty[DefRegister]
    def visit(s: Statement): Unit = { s match { case r: DefRegister => declarations += r; case _ => () }; s.foreachStmt(visit) }
    visit(module.body)
    local.map { path =>
      val name = path.stripPrefix(selected._1 + ".")
      check(declarations.count(_.name == name) == 1, "live storage needs an explicit local register owner: " + path)
      name
    }.toSet
  }
}
