// See LICENSE for license details.
package midas.passes.xilinx

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.transforms.BlackBoxInlineAnno
import midas.passes.DefineAbstractClockGate
import midas.checkpoint.SemanticStateInventory.sha256
import org.json4s._

case class RetiredClockGateInstance(parent: String, instance: String, from: String, to: String)

/** Historical strings only: these are neither live targets nor a substitute
  * for a checkpoint contract for the physical BUFGCE enable latch.
  */
case class XilinxClockGateRetirementAnnotation(
  circuit: String,
  inputSHA256: String,
  rewrittenSHA256: String,
  outputSHA256: String,
  definitions: Seq[(String, String)],
  sourceTarget: String,
  sourceName: String,
  sourceText: String,
  instances: Seq[RetiredClockGateInstance],
) extends NoTargetAnnotation

object ClockGateRetirement {
  private val knownSourceSHA256 = "4b0aadbcd127cc4ceb682c1b439a69fbdaf7f095791021b17b3437b2cde6af48"
  private def check(ok: Boolean, why: String): Unit =
    require(ok, s"Xilinx clock-gate retirement: $why")
  private def name(s: String): Boolean = s != null && s.matches("[A-Za-z_][A-Za-z0-9_$]*")
  private def exact(m: ExtModule): Boolean = m.defname == DefineAbstractClockGate.blackboxName &&
    m.params.isEmpty && m.ports.map(p => (p.name, p.direction, p.tpe)) ==
      DefineAbstractClockGate.blackbox.ports.map(p => (p.name, p.direction, p.tpe))

  private def declarations(c: Circuit): Seq[(String, WDefInstance)] = {
    val rows = scala.collection.mutable.ArrayBuffer.empty[(String, WDefInstance)]
    c.modules.foreach {
      case m: Module =>
        def visit(s: Statement): Unit = { s match {
          case i: WDefInstance => rows += m.name -> i
          case _: DefInstance => check(false, "unresolved instance declaration")
          case _ =>
        }; s.foreachStmt(visit) }
        visit(m.body)
      case _ =>
    }
    rows.toSeq
  }

  // Inspect nested annotations/products, including legacy Named values. Unknown
  // JSON annotations can carry serialized target strings, so inspect those too.
  private def refers(value: Any, retired: Set[String]): Boolean = value match {
    case t: Target => t.moduleOpt.exists(retired) || t.tokens.exists {
      case TargetToken.OfModule(module) => retired(module)
      case _ => false
    }
    case m: ModuleName => retired(m.name)
    case c: ComponentName => retired(c.module.name)
    case s: String if s.startsWith("~") =>
      scala.util.Try(Target.deserialize(s)).toOption.exists(t => refers(t, retired))
    case JString(s) => refers(s, retired)
    case p: Product => p.productIterator.exists(x => refers(x, retired))
    case xs: Iterable[_] => xs.exists(x => refers(x, retired))
    case _ => false
  }

  /** Execute at the actual platform rewrite. Every declaration, including those
    * in unreachable modules, is replaced before removing exact dead definitions.
    */
  def apply(state: CircuitState): CircuitState = {
    check(!state.annotations.exists(_.isInstanceOf[XilinxClockGateRetirementAnnotation]), "already retired")
    val defs = state.circuit.modules.collect {
      case m: ExtModule if m.defname == DefineAbstractClockGate.blackboxName => m
    }
    if (defs.isEmpty) return state
    check(defs.forall(exact), "unknown abstract-gate definition")
    val retired = defs.map(_.name).toSet
    check(!retired(state.circuit.main), "cannot retire circuit root")
    check(state.circuit.modules.map(_.name).distinct.size == state.circuit.modules.size, "duplicate module definition")
    val physical = state.circuit.modules.filter(_.name == DefineBUFGCE.blackboxName)
    check(physical == Seq(DefineBUFGCE.blackbox), "unknown physical BUFGCE definition")
    val sources = state.annotations.collect { case a: BlackBoxInlineAnno if retired(a.target.name) => a }
    check(sources.size == 1, "exactly one known inline source required")
    val source = sources.head
    check(source.target.circuit.name == state.circuit.main &&
      source.name == "AbstractClockGate.v" && source.text == DefineAbstractClockGate.impl &&
      sha256(source.text) == knownSourceSHA256, "unknown abstract-gate inline source")
    val kept = state.annotations.filterNot(_ == source)
    check(!kept.exists(a => refers(a, retired)), "annotation references retiring definition")
    val rows = declarations(state.circuit).collect { case (parent, i) if retired(i.module) =>
      RetiredClockGateInstance(parent, i.name, i.module, DefineBUFGCE.blackboxName)
    }.sortBy(r => (r.parent, r.instance, r.from, r.to))
    check(rows.map(r => r.parent -> r.instance).distinct.size == rows.size, "duplicate instance declaration")
    def rewrite(s: Statement): Statement = s match {
      case i: WDefInstance if retired(i.module) => i.copy(module = DefineBUFGCE.blackboxName)
      case other => other.mapStmt(rewrite)
    }
    val rewritten = state.circuit.copy(modules = state.circuit.modules.map {
      case m: Module => m.copy(body = rewrite(m.body))
      case m => m
    })
    check(!declarations(rewritten).exists { case (_, i) => retired(i.module) }, "retiring definition remains reachable")
    val output = rewritten.copy(modules = rewritten.modules.filterNot(m => retired(m.name)))
    val history = XilinxClockGateRetirementAnnotation(state.circuit.main,
      sha256(state.circuit.serialize), sha256(rewritten.serialize), sha256(output.serialize),
      defs.map(m => m.name -> m.serialize).sortBy(_._1),
      ModuleTarget(source.target.circuit.name, source.target.name).serialize,
      source.name, source.text, rows)
    context(history)
    state.copy(circuit = output, annotations = kept :+ history)
  }

  /** Strict context validation for consumers preserving this inert receipt. */
  def context(a: XilinxClockGateRetirementAnnotation): JValue = {
    check(name(a.circuit) && Seq(a.inputSHA256, a.rewrittenSHA256, a.outputSHA256)
      .forall(s => s != null && s.matches("[0-9a-f]{64}")), "invalid historical identity")
    check(a.definitions.nonEmpty && a.definitions == a.definitions.sortBy(_._1) &&
      a.definitions.map(_._1).distinct.size == a.definitions.size, "invalid historical definitions")
    a.definitions.foreach { case (module, text) =>
      check(name(module) && module != a.circuit, "invalid retired module name")
      val parsed = Parser.parse(s"circuit $module :\n" + text.linesIterator.map("  " + _).mkString("\n")).modules
      check(parsed.size == 1 && (parsed.head match {
        case m: ExtModule => m.name == module && exact(m) && m.serialize == text
        case _ => false
      }), "unknown historical gate definition")
    }
    val names = a.definitions.map(_._1).toSet
    val target = scala.util.Try(Target.deserialize(a.sourceTarget)).toOption
    check(target.exists {
      case t: ModuleTarget => t.circuit == a.circuit && names(t.module)
      case _ => false
    } && a.sourceName == "AbstractClockGate.v" && a.sourceText == DefineAbstractClockGate.impl &&
      sha256(a.sourceText) == knownSourceSHA256, "unknown historical inline source")
    check(a.instances == a.instances.sortBy(r => (r.parent, r.instance, r.from, r.to)) &&
      a.instances.map(r => r.parent -> r.instance).distinct.size == a.instances.size &&
      a.instances.forall(r => name(r.parent) && !names(r.parent) && name(r.instance) &&
        names(r.from) && r.to == DefineBUFGCE.blackboxName), "invalid historical instance mapping")
    JObject(List("producer" -> JString("midas.passes.xilinx.HostSpecialization"),
      "circuit" -> JString(a.circuit), "input_sha256" -> JString(a.inputSHA256),
      "rewritten_sha256" -> JString(a.rewrittenSHA256), "output_sha256" -> JString(a.outputSHA256),
      "definitions" -> JArray(a.definitions.map { case (module, text) => JObject(List(
        "module" -> JString(module), "definition" -> JString(text), "definition_sha256" -> JString(sha256(text)))) }.toList),
      "source_target" -> JString(a.sourceTarget), "source_name" -> JString(a.sourceName),
      "source_text" -> JString(a.sourceText), "source_sha256" -> JString(sha256(a.sourceText)),
      "instances" -> JArray(a.instances.map(r => JObject(List("parent" -> JString(r.parent),
        "instance" -> JString(r.instance), "from" -> JString(r.from), "to" -> JString(r.to)))).toList)))
  }
}
