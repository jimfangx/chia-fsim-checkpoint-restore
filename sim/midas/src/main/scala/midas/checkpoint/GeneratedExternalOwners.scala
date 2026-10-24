// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.{CircuitState, WDefInstance, WDefInstanceConnector}
import firrtl.ir._
import firrtl.transforms.BlackBoxInlineAnno
import org.json4s._

/** Reachability, not the presence of an unused definition, determines storage
  * ownership. Keep external definitions in the source and its hash: this is
  * an audit, not dead-code elimination or permission to discard annotations.
  */
object GeneratedExternalOwners {
  def instances(state: CircuitState, root: String): Seq[(String, DefModule)] = {
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    require(modules.size == state.circuit.modules.size, "external-owner audit: duplicate module definitions")
    val result = mutable.ArrayBuffer.empty[(String, DefModule)]
    def walk(name: String, path: String, ancestors: Set[String]): Unit = {
      require(!ancestors(name), s"external-owner audit: recursive hierarchy at $path")
      val m = modules.getOrElse(name, throw new IllegalArgumentException(s"external-owner audit: missing module $name at $path"))
      result += path -> m
      m match {
        case m: Module =>
          def visit(s: Statement): Unit = {
            s match {
              case i: WDefInstance => walk(i.module, s"$path.${i.name}", ancestors + name)
              case i: WDefInstanceConnector => walk(i.module, s"$path.${i.name}", ancestors + name)
              case _ => ()
            }
            s.foreachStmt(visit)
          }
          visit(m.body)
        case _: ExtModule => ()
        case _ => throw new IllegalArgumentException(s"external-owner audit: unsupported module at $path")
      }
    }
    walk(root, root, Set.empty)
    require(result.map(_._1).distinct.size == result.size, "external-owner audit: duplicate instance hierarchy")
    result.toSeq
  }

  def audit(state: CircuitState, root: String): String = {
    import SemanticStateInventory.{canonical, sha256}
    val live = instances(state, root)
    val reachable = live.map(_._2.name).toSet
    def definition(m: ExtModule): List[(String, JValue)] = {
      val sources = state.annotations.collect { case a: BlackBoxInlineAnno if a.target.name == m.name =>
        JObject(List("circuit" -> JString(a.target.circuit.name), "filename" -> JString(a.name),
          "source_sha256" -> JString(sha256(a.text))))
      }.sortBy(canonical)
      List("module" -> JString(m.name), "defname" -> JString(m.defname),
        "definition_sha256" -> JString(sha256(m.serialize)), "inline_sources" -> JArray(sources.toList))
    }
    val rows = live.collect { case (path, m: ExtModule) =>
      val physical = m.defname == "BUFGCE"
      JObject(List("hierarchy" -> JString(path)) ++ definition(m) ++ List(
        // Existing inventory adapters describe simulator models. In particular
        // metasim's output register is not the physical primitive's CE latch.
        "physical_primitive" -> JBool(physical),
        "physical_capture_binding" -> JString("unsupported"),
        "requires_explicit_owner" -> JBool(true)))
    }
    val unused = state.circuit.modules.collect { case m: ExtModule if !reachable(m.name) => JObject(definition(m)) }
    canonical(JObject(List("audit_version" -> JInt(1), "scope" -> JString("external_owner_admission_only"),
      "root_module" -> JString(root), "reference_sha256" -> JString(sha256(state.circuit.serialize)),
      "deployable_capture_image" -> JBool(false), "complete_fpga_capture" -> JBool(false),
      "reachable_external_instances" -> JArray(rows.sortBy(r => (r \ "hierarchy").values.toString).toList),
      "unused_external_definitions" -> JArray(unused.sortBy(r => (r \ "module").values.toString).toList))))
  }
}
