// See LICENSE for license details.
package midas.checkpoint

import firrtl.WRef
import firrtl.ir._
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Reconcile local-only banks with the SAME original simulator inventory used
  * by subtree regions. Ancestor/local ownership may coexist with child regions;
  * it never claims the children, the transport, or a complete atomic cut.
  */
object NativeCheckpointShellIdentity {
  import SemanticStateInventory.{canonical, sha256}
  case class Result(manifest: String, binding: String, rawClock: String)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "native shell identity: " + why)
  private def statements(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }

  def discover(shell: NativeCheckpointShellStateAccess.Result,
      inventory: Option[ReservedCheckpointInventory] = None): Result = {
    NativeCheckpointShellStateAccess.validate(shell)
    val host = shell.host
    inventory.foreach(_.validateSource(host))
    val storage = host.copy(annotations = host.annotations.filterNot(SimulatorBankProbeContext.accepts))
    val analysis = inventory.map(_.analysisFor(storage)).getOrElse(storage)
    val globalManifest = SimulatorStateInventory.discover(analysis)
    val globalPlan = PhysicalClockCutPlan.discover(analysis, globalManifest)
    val globalDependencies = PhysicalClockCutDependencies.discover(analysis, globalManifest, globalPlan)
    val source = parse(shell.mapping)
    val JString(scope) = source \ "selected_execution"
    val components = scope.split("\\.").toVector
    val executions = GeneratedExternalOwners.instances(host, host.circuit.main).toMap
    val selected = executions(scope).asInstanceOf[Module]

    // Prove every local storage clock through transparent Clock aliases back
    // to the raw enclosing input. A data cast, gate, mux, second clock or child
    // output is not a common low-phase freeze authority.
    def resolve(m: Module, clock: Expression, enable: Option[Expression] = None,
        seen: Set[String] = Set.empty): String = {
      val all = statements(m.body)
      val aliases = all.collect { case n: DefNode => n.name; case w: DefWire => w.name }.toSet
      val drivers = all.collect {
        case c: Connect => c.loc.serialize -> c.expr
        case n: DefNode => n.name -> n.value
      }.groupBy(_._1)
      // Match the existing memory-clock provenance contract: Chisel's
      // validif(enable, clock) is transparent only for this same memory port
      // when its enable is the identical condition (through scalar aliases
      // and the standard mux(enable, 1, 0) encoding).
      def identity(value: Expression, visited: Set[String] = Set.empty): String = {
        val name = value.serialize
        check(!visited(name), "memory enable alias cycle")
        value match {
          case Mux(cond, UIntLiteral(one, IntWidth(w1)), UIntLiteral(zero, IntWidth(w0)), _)
              if one == 1 && zero == 0 && w1 == 1 && w0 == 1 => identity(cond, visited + name)
          case _ if aliases(name) && value.tpe == UIntType(IntWidth(1)) && drivers.contains(name) =>
            val ds = drivers(name)
            check(ds.size == 1, "multiply driven memory enable alias")
            identity(ds.head._2, visited + name)
          case _ => name
        }
      }
      val name = clock.serialize
      check(!seen(name), "clock alias cycle")
      clock match {
        case r: WRef if r.tpe == ClockType &&
            m.ports.exists(p => p.name == r.name && p.direction == Input && p.tpe == ClockType) => r.name
        case r: WRef if r.tpe == ClockType =>
          val ds = drivers.getOrElse(r.name, Nil)
          check(ds.size == 1, "undriven or multiply driven local clock " + m.name + "." + r.name)
          resolve(m, ds.head._2, enable, seen + name)
        case ValidIf(cond, value, ClockType) if enable.exists(e => identity(e) == identity(cond)) =>
          resolve(m, value, enable, seen + name)
        case _ => throw new IllegalArgumentException("native shell identity: nontransparent local clock")
      }
    }
    val local = shell.reference.circuit.modules.head.asInstanceOf[Module]
    val clocks = statements(local.body).flatMap {
      case r: DefRegister => Seq(resolve(selected, r.clock))
      case m: DefMemory => (m.readers ++ m.writers ++ m.readwriters).map { p =>
        val port = m.name + "." + p
        def driver(field: String): Expression = {
          val ds = statements(local.body).collect { case c: Connect if c.loc.serialize == port + "." + field => c.expr }
          check(ds.size == 1, "undriven or multiply driven local memory " + port + "." + field)
          ds.head
        }
        resolve(selected, driver("clk"), Some(driver("en")))
      }
      case _ => Nil
    }.distinct
    check(clocks.size == 1, "local storage must share one raw clock")
    var raw = clocks.head
    components.indices.dropRight(1).reverse.foreach { index =>
      val parent = executions(components.take(index + 1).mkString(".")).asInstanceOf[Module]
      raw = resolve(parent, WRef(components(index + 1) + "." + raw, ClockType))
    }
    check((parse(globalPlan) \ "common_source_clock") == JString(raw), "local clock differs from global raw clock")

    val globals = (parse(globalManifest) \ "state").asInstanceOf[JArray].arr.filterNot(r =>
      (r \ "adapter") == JString("metasim-bufgce-v1"))
    val physical = (parse(globalPlan) \ "physical_gates").asInstanceOf[JArray].arr
    val originals = (parse(shell.manifest) \ "state").asInstanceOf[JArray].arr
    val mappings = (source \ "state").asInstanceOf[JArray].arr
    val byID = globals.map(r => (r \ "state_id") -> r).toMap
    check(byID.size == globals.size, "duplicate global owner")
    val state = shell.entries.zip(mappings).zipWithIndex.map { case ((entry, mapping), ordinal) =>
      val global = byID.getOrElse(JString(entry.stateID),
        throw new IllegalArgumentException("native shell identity: missing original local owner"))
      val logical = originals.find(r => (r \ "state_id") == (mapping \ "local_state_id")).get
      val expected = logical merge JObject(List("state_id" -> JString(entry.stateID), "hierarchy" -> (mapping \ "hierarchy")))
      check(global == expected, "global local storage shape/adapter mismatch")
      check((mapping \ "state_id") == JString(entry.stateID) && (mapping \ "bank") == JInt(entry.bank) &&
        (mapping \ "slot") == JInt(entry.slot) && (mapping \ "transfer_width") == JInt(entry.width) &&
        (mapping \ "transfer_depth") == JInt(entry.depth), "local selector mismatch")
      JObject(List("ordinal" -> JInt(ordinal), "domain" -> JString("functional"),
        "state_id" -> JString(entry.stateID), "selected_state_id" -> (mapping \ "local_state_id"),
        "hierarchy" -> (mapping \ "hierarchy"), "storage_hierarchy" -> (mapping \ "storage_hierarchy"),
        "bank" -> JInt(entry.bank), "slot" -> JInt(entry.slot), "width" -> JInt(entry.width),
        "depth" -> JInt(entry.depth), "logical_state" -> global))
    }
    val ids = shell.entries.map(e => JString(e.stateID): JValue).toSet
    val flags = List("complete_fpga_capture", "deployable_capture_image", "production_freeze_owners_bound",
      "metasim_restore_supported").map(_ -> JBool(false))
    val external = inventory.toList.flatMap(i => List("unsupported_external_owners" -> i.unsupportedOwners,
      "reserved_inventory_binding_sha256" -> JString(sha256(i.binding))))
    val manifest = canonical(JObject(List("identity_version" -> JInt(1),
      "scope" -> JString("selected_shell_global_identities_only"), "ownership_scope" -> JString("local_storage_only"),
      "selected_scope" -> JString(scope), "state" -> JArray(state.toList),
      "uncovered_functional_state" -> JArray(globals.filterNot(r => ids(r \ "state_id"))),
      "uncovered_physical_gates" -> JArray(physical), "physical_clock_gates" -> JInt(0)) ++ flags ++ external))
    val binding = canonical(JObject(List("identity_binding_version" -> JInt(1),
      "manifest_sha256" -> JString(sha256(manifest)), "host_sha256" -> JString(sha256(host.circuit.serialize)),
      "global_simulator_manifest_sha256" -> JString(sha256(globalManifest)),
      "global_clock_plan_sha256" -> JString(sha256(globalPlan)),
      "global_dependencies_sha256" -> JString(sha256(globalDependencies)), "raw_clock" -> JString(raw),
      "shell_binding_sha256" -> JString(sha256(shell.binding))) ++ flags ++ external))
    Result(manifest, binding, raw)
  }
}
