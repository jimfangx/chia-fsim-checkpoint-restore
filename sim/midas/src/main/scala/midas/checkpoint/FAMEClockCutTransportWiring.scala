// See LICENSE for license details.
package midas.checkpoint

import firrtl.{CircuitState, LowForm, WDefInstance, WRef, WSubField}
import firrtl.annotations.{InstanceTarget, JsonProtocol}
import firrtl.ir._
import firrtl.transforms.{BlackBoxInlineAnno, BlackBoxPathAnno, BlackBoxResourceAnno}
import org.json4s._

/** Qualify an EXISTING transport owner with the local FAME witness. This is
  * deliberately not another collector: the original present/frozen/error
  * inputs remain mandatory and all other owner bits are preserved. It owns
  * neither physical parking nor the transport's live registers. Production
  * image admission therefore remains unchanged.
  */
object FAMEClockCutTransportWiring {
  import SemanticStateInventory.{canonical, sha256}
  val prefix = "checkpoint_fame_transport_"
  case class Result(state: CircuitState, binding: String, witness: FAMEClockCutWiring.Witness)
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "FAME transport witness: " + why)
  private def all(s: Statement): Seq[Statement] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Statement]
    def visit(x: Statement): Unit = { out += x; x.foreachStmt(visit) }
    visit(s); out.toSeq
  }
  private def resource(name: String): String = {
    val in = getClass.getResourceAsStream("/" + name)
    check(in != null, "missing transport source " + name)
    try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
  }

  /** The target must name the one actual external transport execution, after
    * native widget inlining, not a guessed flattened instance name. Owner
    * ordering is an explicit caller contract recorded in the receipt. The
    * original independent owner inputs may not be constants or aliases of
    * each other; this API never manufactures an acknowledgement.
    */
  def connect(input: CircuitState, target: InstanceTarget, primaryDomain: String,
      ownerNames: Seq[String], fameOwner: String): Result = {
    check(input.form == LowForm, "resolved LowForm required")
    check(ownerNames.nonEmpty && ownerNames.size < 65535 && ownerNames.distinct.size == ownerNames.size &&
      ownerNames.forall(_.matches("[A-Za-z0-9_.:/-]+")) && ownerNames.contains(fameOwner),
      "explicit unique ordered owners and selected FAME owner required")
    check(target.circuit == input.circuit.main && target.module == input.circuit.main && target.asPath.nonEmpty,
      "transport target must be rooted at the actual circuit")
    val witness = FAMEClockCutWiring.generateWitness(input, primaryDomain)
    val state = witness.state
    val live = GeneratedExternalOwners.instances(state, state.circuit.main)
    val path = (Seq(target.module) ++ target.asPath.map(_._1.value)).mkString(".")
    var scope = state.circuit.main
    target.asPath.foreach { case (instance, module) =>
      scope += "." + instance.value
      check(live.exists { case (p, m) => p == scope && m.name == module.value }, "stale intermediate transport target")
    }
    val transports = live.collect { case (p, m: ExtModule)
      if Set("CheckpointPhysicalCaptureTransport", "CheckpointNativeCaptureTransport")(m.defname) => p -> m }
    check(transports.size == 1 && transports.head._1 == path && transports.head._2.name == target.ofModule,
      "one exact existing transport execution required")
    val transport = transports.head._2
    val executions = input.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }
    check(executions.size <= 1 && executions.forall(_.owners == ownerNames), "existing widget owner order differs")
    val native = transport.defname == "CheckpointNativeCaptureTransport"
    val sources = if (native) midas.widgets.CheckpointNativeCaptureTransportBlackBox.resources else Seq(
      "CheckpointPhysicalCaptureTransport.sv", "CheckpointPhysicalClockCut.sv", "CheckpointCaptureTransport.sv",
      "CheckpointCaptureCut.sv")
    val inline = state.annotations.collect { case a: BlackBoxInlineAnno if a.target.name == transport.name => a }
    check(inline.size == sources.size && inline.map(_.name).toSet == sources.toSet &&
      inline.forall(a => a.target.circuit.name == state.circuit.main && a.text == resource(a.name)) &&
      !state.annotations.exists {
        case a: BlackBoxPathAnno => a.target.name == transport.name
        case a: BlackBoxResourceAnno => a.target.name == transport.name
        case _ => false
      }, "unresolved, duplicate or changed transport source")
    val ps = transport.params.collect { case IntParam(n, v) => n -> v }.toMap
    val keys = if (native) Set("BITS", "GATES", "OWNERS", "ENTRIES", "EXTERNAL_CLOCK_NETWORK", "SNAPSHOT_BANK",
      "NATIVE_CONTROL_BANK", "CAPTURE_NATIVE_CONTROL", "MANIFEST_SHA256") else Set("GATES", "OWNERS", "ENTRIES",
      "EXTERNAL_CLOCK_NETWORK", "RESTORE_ENABLE", "MANIFEST_SHA256")
    check(ps.keySet == keys && transport.params.size == keys.size && ps("OWNERS") == ownerNames.size &&
      ps("EXTERNAL_CLOCK_NETWORK") == 1 && ps("GATES") > 0 && ps("GATES") <= 65535 &&
      ps("ENTRIES") > 0 && ps("ENTRIES") <= 65535 && ps("MANIFEST_SHA256") > 0 &&
      ps("MANIFEST_SHA256") < (BigInt(1) << 256), "invalid transport parameters or owner ordering geometry")
    if (native) check(ps("BITS") > 0 && ps("BITS") + ps("GATES") + 694 <= Int.MaxValue &&
      Set(BigInt(0), BigInt(1))(ps("CAPTURE_NATIVE_CONTROL")) &&
      ps("SNAPSHOT_BANK") >= 0 && ps("SNAPSHOT_BANK") < 255 && ps("NATIVE_CONTROL_BANK") >= 0 &&
      ps("NATIVE_CONTROL_BANK") < 255 && ps("SNAPSHOT_BANK") != ps("NATIVE_CONTROL_BANK"), "invalid native geometry")
    else check(ps("RESTORE_ENABLE") == 0, "parked restore transport cannot supply a running witness")
    val owners = ownerNames.size; val gates = ps("GATES").toInt
    val bit = UIntType(IntWidth(1)); val word = UIntType(IntWidth(64)); val vector = UIntType(IntWidth(owners))
    def abi(xs: Seq[(String, BigInt)], d: Direction): Seq[Port] =
      xs.map { case (n, w) => Port(NoInfo, n, d, UIntType(IntWidth(w))) }
    val commonIn: Seq[(String, BigInt)] = Seq("reset" -> 1, "target_cycle" -> 64,
      "owner_present" -> owners, "owner_frozen" -> owners, "owner_error" -> owners,
      "mmio_valid" -> 1, "mmio_write" -> 1, "mmio_address" -> 8, "mmio_wdata" -> 32,
      "mmio_response_ready" -> 1, "descriptor_valid" -> 1, "descriptor_bank" -> 8,
      "descriptor_slot" -> 16, "descriptor_width" -> 16, "descriptor_depth" -> 32,
      "bank_request_ready" -> 1, "bank_response_valid" -> 1, "bank_response_data" -> 64,
      "bank_response_error" -> 1, "dma_ready" -> 1, "ce" -> gates)
    val commonOut: Seq[(String, BigInt)] = Seq("freeze" -> 1, "mmio_ready" -> 1, "mmio_response_valid" -> 1,
      "mmio_rdata" -> 32, "mmio_response_error" -> 1, "descriptor_entry" -> 32, "bank_request_valid" -> 1,
      "bank" -> 8, "slot" -> 16, "address" -> 32, "bank_response_ready" -> 1, "dma_valid" -> 1,
      "dma_data" -> 512, "dma_last" -> 1, "dma_abort" -> 1, "gate_clock" -> gates,
      "clock_mask" -> 1, "captured_ce" -> gates)
    val extraIn: Seq[(String, BigInt)] = if (native) Seq("reader_idle" -> 1, "live_state" -> ps("BITS")) else
      Seq("restore_hold" -> 1, "restore_load_pos" -> 1, "restore_load_neg" -> 1, "restore_state" -> (481 + gates))
    val extraOut: Seq[(String, BigInt)] = if (native) Seq("functional_hold" -> 1, "native_valid" -> 1,
      "native_failed" -> 1, "native_cut_cycle" -> 64, "snapshot_state" -> ps("BITS"),
      "control_state" -> (ps("BITS") + gates + 627), "native_control_bank_state" -> 67) else
      Seq("physical_stopped" -> 1, "physical_read_enable" -> 1, "physical_failed" -> 1,
        "physical_cut_cycle" -> 64, "physical_error" -> 3, "control_state" -> (481 + gates))
    val expected = Seq(Port(NoInfo, "clock", Input, ClockType), Port(NoInfo, "root_clock", Output, ClockType)) ++
      abi(commonIn ++ extraIn, Input) ++ abi(commonOut ++ extraOut, Output)
    check(transport.ports.size == expected.size && transport.ports.map(_.copy(info = NoInfo)).toSet == expected.toSet,
      "transport differs from complete public ABI")
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val chain = (Seq(state.circuit.main) ++ target.asPath.dropRight(1).map(_._2.value)).map { name =>
      check(live.count(_._2.name == name) == 1 && modules(name).isInstanceOf[Module],
        "routing ancestor must have one concrete execution: " + name)
      val m = modules(name).asInstanceOf[Module]
      check(!(m.ports.map(_.name) ++ all(m.body).collect { case d: IsDeclaration => d.name })
        .exists(_.startsWith(prefix)), "reserved route identifier collision")
      m
    }
    val host = chain.last; val instance = target.asPath.last._1.value
    val hostPath = path.split('.').dropRight(1).mkString(".")
    def driver(m: Module, name: String): Expression = {
      val ds = all(m.body).collect {
        case c: Connect if c.loc.serialize == name => c.expr
        case n: DefNode if n.name == name => n.value
      }
      check(ds.size == 1, "missing or multiple drivers: " + m.name + "." + name); ds.head
    }
    def localInput(m: Module, e: Expression, tpe: Type, seen: Set[String] = Set.empty): String = e match {
      case r: WRef if !seen(r.name) =>
        if (m.ports.exists(p => p.name == r.name && p.direction == Input && p.tpe == tpe)) r.name
        else {
          check(all(m.body).exists {
            case w: DefWire => w.name == r.name && w.tpe == tpe
            case n: DefNode => n.name == r.name && n.value.tpe == tpe
            case _ => false
          }, "input route crosses non-alias storage or wrong type")
          localInput(m, driver(m, r.name), tpe, seen + r.name)
        }
      case _ => throw new IllegalArgumentException("FAME transport witness: independent explicit input alias required")
    }
    val pins = Seq("owner_present", "owner_frozen", "owner_error")
    val original = pins.map(n => n -> driver(host, instance + "." + n)).toMap
    pins.foreach(n => localInput(host, original(n), vector))
    val oldCycle = driver(host, instance + ".target_cycle")
    localInput(host, oldCycle, word)
    // The FAME observer and the existing transport must use the same actual
    // root clock. Follow local aliases and input ports across every real hop.
    def rootInput(scope: String, expression: Expression, tpe: Type, seen: Set[String] = Set.empty): String = expression match {
      case r: WRef =>
        val module = live.find(_._1 == scope).get._2.asInstanceOf[Module]
        val key = scope + "." + r.name; check(!seen(key), "clock alias cycle")
        if (module.ports.exists(p => p.name == r.name && p.direction == Input && p.tpe == tpe)) {
          if (scope == state.circuit.main) key
          else {
            val parent = scope.split('.').dropRight(1).mkString(".")
            val pm = live.find(_._1 == parent).get._2.asInstanceOf[Module]
            rootInput(parent, driver(pm, scope.split('.').last + "." + r.name), tpe, seen + key)
          }
        } else {
          check(all(module.body).exists {
            case w: DefWire => w.name == r.name && w.tpe == tpe
            case n: DefNode => n.name == r.name && n.value.tpe == tpe
            case _ => false
          }, "root input route crosses non-alias storage or wrong type")
          rootInput(scope, driver(module, r.name), tpe, seen + key)
        }
      case _ => throw new IllegalArgumentException("FAME transport witness: independent root input alias required")
    }
    val origins = pins.map(n => rootInput(hostPath, original(n), vector))
    check(origins.distinct.size == pins.size, "owner present/frozen/error root inputs alias")
    val clockRow = FAMEClockProgressBindings.resolve(state).find(r =>
      (r \ "role") == JString("host_clock")).get
    val JString(clockPath) = clockRow \ "rtl_path"
    check(rootInput(hostPath, driver(host, instance + ".clock"), ClockType) ==
      rootInput(clockPath.split('.').dropRight(1).mkString("."), WRef(clockPath.split('.').last, ClockType), ClockType),
      "transport and FAME observer raw clocks differ")
    val ownerBit = ownerNames.indexOf(fameOwner)
    def qualified(name: String): Expression = {
      val old = WRef(prefix + name, vector)
      val selected = DoPrim(firrtl.PrimOps.Bits, Seq(old), Seq(BigInt(ownerBit), BigInt(ownerBit)), bit)
      val valid = WRef(if (chain.size == 1) FAMEClockCutWiring.valid else prefix + "consistent", bit)
      val changed = if (name == "owner_error") DoPrim(firrtl.PrimOps.Or,
        Seq(selected, DoPrim(firrtl.PrimOps.Not, Seq(valid), Nil, bit)), Nil, bit)
      else DoPrim(firrtl.PrimOps.And, Seq(selected, valid), Nil, bit)
      (0 until owners).reverse.map { index =>
        if (index == ownerBit) changed else DoPrim(firrtl.PrimOps.Bits, Seq(old), Seq(BigInt(index), BigInt(index)), bit)
      }.reduceLeft((a, b) => DoPrim(firrtl.PrimOps.Cat, Seq(a, b), Nil,
        UIntType(IntWidth(a.tpe.asInstanceOf[UIntType].width.asInstanceOf[IntWidth].width +
          b.tpe.asInstanceOf[UIntType].width.asInstanceOf[IntWidth].width))))
    }
    val routed = chain.zipWithIndex.map { case (m, index) =>
      val root = index == 0; val last = index == chain.size - 1
      val names = Seq("consistent" -> bit, "applied_cycle" -> word)
      def signal(n: String, t: Type) = WRef(if (root) FAMEClockCutWiring.prefix + n else prefix + n, t)
      val next = if (last) Nil else names.map { case (n, t) =>
        val child = target.asPath(index)._1.value
        Connect(NoInfo, WSubField(WRef(child), prefix + n, t), signal(n, t))
      }
      def rewrite(s: Statement): Statement = s match {
        case c: Connect if last && c.loc.serialize == instance + ".target_cycle" =>
          c.copy(expr = signal("applied_cycle", word))
        case c: Connect if last && pins.exists(n => c.loc.serialize == instance + "." + n) =>
          c.copy(expr = qualified(c.loc.serialize.stripPrefix(instance + ".")))
        case x => x.mapStmt(rewrite)
      }
      // The original owner expression may name a DefNode in the original body.
      // Declare the saved wires before their qualified uses, but assign them
      // only after the original aliases have been declared. Hoisting both
      // together would create a forward reference in the real inlined widget.
      val savedWires = if (!last) Nil else pins.map(n => DefWire(NoInfo, prefix + n, vector))
      val savedConnections = if (!last) Nil else pins.map(n => Connect(NoInfo, WRef(prefix + n, vector), original(n)))
      val extraPorts = if (root) Nil else names.map { case (n, t) => Port(NoInfo, prefix + n, Input, t) }
      m.name -> m.copy(ports = m.ports ++ extraPorts,
        body = Block(savedWires ++ Seq(rewrite(m.body)) ++ savedConnections ++ next))
    }.toMap
    val circuit = state.circuit.copy(modules = state.circuit.modules.map(m => routed.getOrElse(m.name, m)))
    val typed = firrtl.passes.InferTypes.run(firrtl.passes.ResolveKinds.run(firrtl.passes.ToWorkingIR.run(circuit)))
    val flowed = firrtl.passes.ResolveFlows.run(typed)
    firrtl.passes.CheckTypes.run(flowed); firrtl.passes.CheckFlows.run(flowed)
    val result = state.copy(circuit = flowed)
    val binding = canonical(JObject(List("binding_version" -> JInt(1),
      "scope" -> JString("existing_transport_local_fame_witness_qualification_only"),
      "reference_sha256" -> JString(sha256(input.circuit.serialize)),
      "reference_annotations_sha256" -> JString(sha256(JsonProtocol.serialize(input.annotations))),
      "witness_binding_sha256" -> JString(sha256(witness.binding)),
      "wired_reference_sha256" -> JString(sha256(result.circuit.serialize)),
      "transport" -> JString(target.serialize), "transport_definition_sha256" -> JString(sha256(transport.serialize)),
      "transport_sources" -> JObject(sources.map(n => n -> JString(sha256(resource(n)))).toList),
      "owners" -> JArray(ownerNames.map(JString(_)).toList), "fame_owner_bit" -> JInt(ownerBit),
      "owner_order_provenance" -> JString(if (executions.nonEmpty) "existing_widget_execution_annotation"
        else "explicit_caller_order_not_independently_owned"),
      "primary_domain" -> JString(primaryDomain), "label_role" -> JString("applied"),
      "original_cycle_source" -> JString(oldCycle.serialize),
      "original_owner_inputs" -> JObject(pins.map(n => n -> JString(original(n).serialize)).toList),
      "root_owner_inputs" -> JObject(pins.zip(origins).map { case (n, p) => n -> JString(p) }.toList),
      "independent_owner_acknowledgements_required" -> JBool(true), "new_storage_bits" -> JInt(0),
      "new_collectors" -> JInt(0), "new_transports" -> JInt(0),
      "global_target_cut_proven" -> JBool(false), "production_freeze_owners_bound" -> JBool(false),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "metasim_restore_supported" -> JBool(false))))
    Result(result, binding, witness)
  }

  def validate(input: CircuitState, target: InstanceTarget, primaryDomain: String,
      ownerNames: Seq[String], fameOwner: String, result: Result): Unit = {
    val expected = connect(input, target, primaryDomain, ownerNames, fameOwner)
    FAMEClockCutWiring.validateWitness(input, primaryDomain, result.witness)
    check(expected.binding == result.binding && expected.state.circuit.serialize == result.state.circuit.serialize &&
      expected.state.form == result.state.form && expected.state.annotations.toSeq == result.state.annotations.toSeq &&
      expected.state.renames.map(_.serialize.linesIterator.toSeq.sorted) ==
        result.state.renames.map(_.serialize.linesIterator.toSeq.sorted), "binding differs from independent regeneration")
  }
}
