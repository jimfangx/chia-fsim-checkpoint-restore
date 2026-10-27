// See LICENSE for license details.
package midas.checkpoint

import scala.collection.mutable
import firrtl.CircuitState
import firrtl.annotations.{Annotation, CircuitName, ComponentName, IsModule, JsonProtocol, MemoryFileInlineAnnotation, MemorySynthInit, MemoryNoSynthInit, ModuleName, ModuleTarget, ReferenceTarget, TargetToken}
import firrtl.ir._
import firrtl.transforms.DontTouchAnnotation
import org.json4s._
import midas.InternalXDCAnnotation
import midas.core.TargetBoxAnnotation
import midas.targetutils.FirrtlMemModelAnnotation
import midas.targetutils.xdc.{RAMStyleHint, RAMStyles, XDCAnnotation, XDCDestinationFile, XDCFiles}

/** Explicit annotation contracts for instance-specialized storage. Never copy
  * an annotation with an old module target or silently drop an unknown owner.
  * A local target applies to every reachable instance of its module; a nonlocal
  * target applies only to the named subtree in each instance of its parent.
  */
private[checkpoint] object GeneratedStateBankAnnotations {
  val translationVersion = 2
  case class Clone(originalModule: String, originalPath: String, module: String, body: Module)
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint bank unsupported: $why")
  // These are global memory-lowering choices, not hierarchy targets. Preserve
  // them for downstream emitters and include them in both annotation hashes.
  // Default RUW must already be resolved in the inventoried IR: applying it
  // after banking could silently select a different read-pipeline owner.
  private def memoryPolicy(a: Annotation): Boolean = a match {
    case firrtl.passes.memlib.InferReadWriteAnnotation |
         firrtl.passes.memlib.DefaultReadFirstAnnotation |
         firrtl.passes.memlib.DefaultWriteFirstAnnotation |
         firrtl.passes.memlib.PassthroughSimpleSyncReadMemsAnnotation => true
    case _ => false
  }
  private[checkpoint] def supported(a: Annotation): Boolean = a match {
    case a if memoryPolicy(a) => true
    case _: DontTouchAnnotation | _: MemoryFileInlineAnnotation | _: firrtl.transforms.CombinationalPath |
         _: firrtl.transforms.NoDedupAnnotation | _: firrtl.passes.InlineAnnotation |
         _: SynthesisPlusArgConstantAnnotation | _: FirrtlMemModelAnnotation |
         _: InternalXDCAnnotation | _: XDCAnnotation | _: TargetBoxAnnotation |
         _: firrtl.transforms.DedupedResult => true
    case MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation => true
    case _ => false
  }
  private def declarations(m: Module): Seq[IsDeclaration] = {
    val all = mutable.ArrayBuffer.empty[IsDeclaration]
    def visit(s: Statement): Unit = {
      s match { case d: IsDeclaration => all += d; case _ => () }
      s.foreachStmt(visit)
    }
    visit(m.body); all.toSeq
  }

  // TargetBox identifies the linked target hierarchy, not a storage word.
  // Preserve the root and every instance hop; flattening to a leaf module
  // loses the distinction between repeated instances. Only concrete reachable
  // paths can be specialized: a stale pre-link placeholder must fail closed.
  private def validateTargetBox(state: CircuitState, target: IsModule,
      modules: Map[String, DefModule], reachable: Set[String], label: String = "target-box"): Unit = {
    if (target.circuit != state.circuit.main) fail(s"foreign $label target: ${target.serialize}")
    def concrete(name: String): Module = modules.get(name) match {
      case Some(m: Module) if reachable(name) => m
      case _ => fail(s"$label target requires a reachable concrete module: $name")
    }
    var owner = concrete(target.module)
    target.asPath.foreach { case (instance, child) =>
      if (!declarations(owner).exists {
        case i: firrtl.WDefInstance => i.name == instance.value && i.module == child.value
        case _ => false
      }) fail(s"$label instance path mismatch: ${target.serialize}")
      owner = concrete(child.value)
    }
  }

  /** XDC arguments are correlated under one explicit root. Never expand them
    * independently: a Cartesian product would constrain unrelated instances.
    * Only exact ground declarations, concrete instance ports, RAMStyleHint
    * logical memories, and the exact
    * platform BUFGCE's public pins survive this contract. A physical constraint
    * is not permission to inventory or capture a vendor-owned latch.
    */
  private def validateXDC(state: CircuitState, destination: XDCDestinationFile,
                          format: String, args: Seq[ReferenceTarget]): Unit = {
    if ("\\{}".r.findAllIn(format).size != args.size) fail("XDC argument/specifier count mismatch")
    val ramStyles = Seq(RAMStyles.ULTRA, RAMStyles.BLOCK, RAMStyles.REGISTERS,
      RAMStyles.DISTRIBUTED, RAMStyles.MIXED).map(RAMStyleHint.propertyTemplate).toSet
    val ramHint = ramStyles(format)
    if (ramHint && (destination != XDCFiles.Synthesis || args.size != 1))
      fail("RAM-style XDC requires one logical memory and the synthesis destination")
    if (args.isEmpty) return
    if (args.map(_.module).distinct.size != 1) fail("XDC arguments require a common explicit root")
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    val reachable = GeneratedExternalOwners.instances(state, state.circuit.main).map(_._2.name).toSet
    def ownerModule(name: String): DefModule = modules.get(name) match {
      case Some(m: Module) if reachable(name) => m
      case Some(m: ExtModule) if reachable(name) && m.defname == "BUFGCE" =>
        PhysicalClockGateContract.validate(state, m); m
      case _ => fail(s"XDC target requires a reachable concrete module: $name")
    }
    def declared(m: DefModule): Seq[IsDeclaration] = m match {
      case m: Module => declarations(m)
      case _: ExtModule => Nil // Never expose vendor implementation internals.
    }
    def ground(t: Type): Boolean = t match {
      case ClockType | ResetType | AsyncResetType | UIntType(IntWidth(_)) | SIntType(IntWidth(_)) => true
      case _ => false
    }
    args.foreach { a =>
      if (a.circuit != state.circuit.main) fail(s"foreign XDC target: ${a.serialize}")
      var owner = ownerModule(a.module)
      a.path.foreach { case (instance, child) =>
        if (!declared(owner).exists {
          case i: firrtl.WDefInstance => i.name == instance.value && i.module == child.value
          case _ => false
        }) fail(s"XDC instance path mismatch: ${a.serialize}")
        owner = ownerModule(child.value)
      }
      val declaration = declared(owner).find(_.name == a.ref)
      // The native RAM hint selects Vivado memory cells by a regexp suffix.
      // Do not generalize this to arbitrary snippets, memory ports, words or
      // registers. Reject regex metacharacters in every interpolated segment.
      val valid = if (ramHint) {
        val names = a.path.map(_._1.value) :+ a.ref
        a.component.isEmpty && names.forall(_.matches("[A-Za-z_][A-Za-z0-9_]*")) &&
          declaration.exists(_.isInstanceOf[DefMemory])
      } else a.component match {
        case Nil => owner.ports.exists(p => p.name == a.ref && ground(p.tpe)) || declaration.exists {
          case d: DefRegister => ground(d.tpe)
          case d: DefWire => ground(d.tpe)
          case d: DefNode => ground(d.value.tpe)
          case i: firrtl.WDefInstance => ownerModule(i.module); true
          case _ => false
        }
        case Seq(TargetToken.Field(port)) => declaration.exists {
          case i: firrtl.WDefInstance => ownerModule(i.module).ports.exists(p => p.name == port && ground(p.tpe))
          case _ => false
        }
        case _ => false
      }
      if (!valid) fail(s"XDC target requires an exact ground declaration or concrete instance port: ${a.serialize}")
    }
  }

  def preflight(state: CircuitState): Unit = {
    GeneratedSynthesisConstantAdapter.validate(state, instrumented = true)
    val unsupported = state.annotations.filterNot(supported)
    if (unsupported.nonEmpty) fail("input annotations require explicit clone/rename translation: " +
      unsupported.map(_.getClass.getName).distinct.mkString(", "))
    if (state.annotations.contains(MemorySynthInit) && state.annotations.contains(MemoryNoSynthInit))
      fail("conflicting memory synthesis initialization policies")
    val policies = state.annotations.filter(memoryPolicy)
    if (policies.distinct.size != policies.size) fail("duplicate memory lowering policy")
    val defaults = policies.filter {
      case firrtl.passes.memlib.DefaultReadFirstAnnotation | firrtl.passes.memlib.DefaultWriteFirstAnnotation => true
      case _ => false
    }
    if (defaults.size > 1) fail("conflicting default read-under-write policies")
    if (defaults.nonEmpty) state.circuit.modules.foreach {
      case m: Module => declarations(m).foreach {
        case mem: DefMemory if mem.readLatency > 0 && mem.readUnderWrite == ReadUnderWrite.Undefined =>
          fail(s"default read-under-write policy must be applied before inventory: ${m.name}.${mem.name}")
        case _ => ()
      }
      case _ => ()
    }
    // The resolver may omit unreachable definitions before specialization.
    // A preservation/initialization contract on one must fail here, rather
    // than be deleted by a compiler rename or an invalid-target cleanup pass.
    val reachable = GeneratedExternalOwners.instances(state, state.circuit.main).map(_._2.name).toSet
    // A production circuit has thousands of deduplication-history targets.
    // They all refer to this immutable state: rebuilding its complete execution
    // hierarchy for every target multiplies the audit cost without adding a
    // check. This map lives only for this preflight, never across circuit edits.
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    def moduleTarget(circuit: String, module: String, serialized: String): Unit = {
      if (circuit != state.circuit.main) fail(s"foreign annotation target: $serialized")
      if (!reachable(module)) fail(s"annotation target is unreachable: $serialized")
      if (!state.circuit.modules.exists { case m: Module => m.name == module; case _ => false })
        fail(s"opaque annotation target: $serialized")
    }
    state.annotations.foreach {
      // FIRRTL deliberately keeps `original` unchanged across renames: it is
      // a historical module identity, possibly in the pre-link circuit. Only
      // `duplicate` refers to the current hierarchy. Never rewrite the origin
      // as a present-day module or flatten a duplicate's instance path.
      case a: firrtl.transforms.DedupedResult =>
        if (a.original.circuit.isEmpty || a.original.module.isEmpty ||
            !a.index.isFinite || (a.duplicate.nonEmpty && (a.index < 0 || a.index > 1)) ||
            (a.duplicate.isEmpty && a.index != -1)) fail("invalid deduplication history")
        a.duplicate.foreach(validateTargetBox(state, _, modules, reachable, "deduplication"))
      case a: TargetBoxAnnotation => validateTargetBox(state, a.target, modules, reachable)
      case a: firrtl.transforms.NoDedupAnnotation =>
        moduleTarget(a.target.circuit, a.target.module, a.target.serialize)
      case a: firrtl.passes.InlineAnnotation => a.target match {
        case c: CircuitName =>
          if (c.name != state.circuit.main) fail(s"foreign annotation target: ${c.serialize}")
        case m: ModuleName => moduleTarget(m.circuit.name, m.name, m.serialize)
        case c: ComponentName =>
          moduleTarget(c.module.circuit.name, c.module.name, c.serialize)
          val m = state.circuit.modules.collectFirst { case m: Module if m.name == c.module.name => m }.get
          var concreteInstance = false
          def visit(s: Statement): Unit = {
            s match {
              case i: firrtl.WDefInstance if i.name == c.name =>
                moduleTarget(c.module.circuit.name, i.module, c.serialize)
                concreteInstance = true
              case _ => ()
            }
            s.foreachStmt(visit)
          }
          visit(m.body)
          if (!concreteInstance) fail(s"inline annotation requires an exact concrete instance: ${c.serialize}")
        case target => fail(s"inline annotation requires a circuit, module name or exact instance name: ${target.serialize}")
      }
      case a: DontTouchAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case a: InternalXDCAnnotation => validateXDC(state, a.destinationFile, a.formatString, a.argumentList)
      case a: XDCAnnotation => validateXDC(state, a.destinationFile, a.formatString, a.argumentList)
      case a: FirrtlMemModelAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case a: MemoryFileInlineAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case a: SynthesisPlusArgConstantAnnotation if !reachable(a.target.module) =>
        fail(s"annotation target is unreachable: ${a.target.serialize}")
      case _ => ()
    }
  }
  def annotationJSON(annotations: Seq[Annotation]): String = JsonProtocol.serialize(annotations.filter {
    case a if memoryPolicy(a) => true
    case _: DontTouchAnnotation | _: MemoryFileInlineAnnotation | _: SynthesisPlusArgConstantAnnotation |
         _: firrtl.transforms.NoDedupAnnotation | _: firrtl.passes.InlineAnnotation |
         _: FirrtlMemModelAnnotation | _: InternalXDCAnnotation | _: XDCAnnotation | _: TargetBoxAnnotation |
         _: firrtl.transforms.DedupedResult => true
    case MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation => true
    case _ => false
  }.distinct.sortBy(_.serialize))
  def translatedHash(state: CircuitState): String = SemanticStateInventory.sha256(annotationJSON(state.annotations))

  def translate(state: CircuitState, clones: Seq[Clone]): (Seq[Annotation], JValue) = {
    preflight(state)
    val modules = state.circuit.modules.map(m => m.name -> m).toMap
    def targets(target: ReferenceTarget, memory: Boolean): Seq[ReferenceTarget] = {
      if (target.circuit != state.circuit.main || target.component.nonEmpty)
        fail(s"annotation requires an exact ground declaration target: ${target.serialize}")
      val parents = clones.filter(_.originalModule == target.module)
      if (parents.isEmpty) fail(s"annotation target is unreachable: ${target.serialize}")
      var leaf = target.module
      target.path.foreach { case (instance, child) =>
        val m = modules.get(leaf) match { case Some(m: Module) => m; case _ => fail("opaque annotation path") }
        if (!declarations(m).exists {
          case i: firrtl.WDefInstance => i.name == instance.value && i.module == child.value
          case _ => false
        }) fail(s"annotation instance path mismatch: ${target.serialize}")
        leaf = child.value
      }
      parents.map { parent =>
        val path = (Seq(parent.originalPath) ++ target.path.map(_._1.value)).mkString(".")
        val clone = clones.find(_.originalPath == path).getOrElse(fail(s"missing annotation clone: $path"))
        val declared = declarations(clone.body).find(_.name == target.ref)
        val valid = if (memory) declared.exists(_.isInstanceOf[DefMemory]) else
          declared.exists {
            case _: DefRegister | _: DefMemory | _: DefWire | _: DefNode => true
            case _ => false
          } || clone.body.ports.exists(_.name == target.ref)
        if (!valid) fail(s"annotation declaration/kind mismatch: ${target.serialize}")
        ReferenceTarget(state.circuit.main, clone.module, Nil, target.ref, Nil)
      }
    }
    val initTargets = mutable.Set.empty[ReferenceTarget]
    val mappings = mutable.ArrayBuffer.empty[JValue]
    def moduleClones(circuit: String, module: String): Seq[Clone] = {
      if (circuit != state.circuit.main) fail(s"foreign annotation module: $circuit.$module")
      val copies = clones.filter(_.originalModule == module)
      if (copies.isEmpty) fail(s"annotation module is unreachable: $module")
      copies
    }
    def xdcCopies(args: Seq[ReferenceTarget]): Seq[Seq[ReferenceTarget]] = {
      if (args.isEmpty) return Seq(Nil)
      // A shared primitive definition may gain NEW executions (the root mask
      // gate). Anchor its local pin constraints to the ORIGINAL executions,
      // using the concrete root and renamed ancestor hops. Keeping a local
      // primitive target would silently broaden the constraint after wiring.
      // All arguments of one annotation follow the same physical execution.
      modules(args.head.module) match {
        case m: ExtModule =>
          PhysicalClockGateContract.validate(state, m)
          val root = clones.filter(_.originalPath == state.circuit.main)
          if (root.size != 1 || root.head.originalModule != state.circuit.main)
            fail("physical XDC requires exactly one original circuit root clone")
          val live = GeneratedExternalOwners.instances(state, state.circuit.main).toMap
          return live.toSeq.filter(_._2.name == m.name).sortBy(_._1).map { case (execution, _) =>
            var path = state.circuit.main
            val hops = execution.stripPrefix(state.circuit.main + ".").split("\\.").toSeq.map { instance =>
              path += "." + instance
              val original = live.getOrElse(path, fail(s"missing physical XDC execution: $path"))
              val emitted = original match {
                case _: ExtModule => original.name
                case _ => clones.find(_.originalPath == path).map(_.module)
                  .getOrElse(fail(s"missing physical XDC ancestor clone: $path"))
              }
              (TargetToken.Instance(instance), TargetToken.OfModule(emitted))
            }
            args.map(a => a.copy(module = root.head.module, path = hops))
          }
        case _ => ()
      }
      moduleClones(args.head.circuit, args.head.module).map { parent =>
        args.map { a =>
          var path = parent.originalPath
          val renamedPath = a.path.map { case (instance, childModule) =>
            path = s"$path.${instance.value}"
            val emittedModule = modules(childModule.value) match {
              case m: ExtModule => PhysicalClockGateContract.validate(state, m); m.name
              case _ => clones.find(_.originalPath == path).map(_.module).getOrElse(fail(s"missing XDC clone: $path"))
            }
            (instance, TargetToken.OfModule(emittedModule))
          }
          a.copy(module = parent.module, path = renamedPath)
        }
      }
    }
    def record(source: Annotation, targets: Seq[String]): Unit =
      mappings += JObject(List("source" -> JString(source.serialize),
        "targets" -> JArray(targets.map(JString(_)).toList)))
    def specialize(target: IsModule): Seq[IsModule] =
      moduleClones(target.circuit, target.module).map { parent =>
        var originalPath = parent.originalPath
        var renamed: IsModule = ModuleTarget(state.circuit.main, parent.module)
        target.asPath.foreach { case (instance, _) =>
          originalPath = s"$originalPath.${instance.value}"
          val child = clones.find(_.originalPath == originalPath).getOrElse(
            fail(s"missing hierarchy clone: $originalPath"))
          renamed = renamed.instOf(instance.value, child.module)
        }
        renamed
      }
    val requested = state.annotations.flatMap {
      case a: firrtl.transforms.DedupedResult =>
        val copies = a.duplicate match {
          case Some(target) => specialize(target).map(t => a.copy(duplicate = Some(t)))
          case None => Seq(a)
        }
        record(a, copies.map(_.serialize))
        copies
      case a: TargetBoxAnnotation =>
        val copies = moduleClones(a.target.circuit, a.target.module).map { parent =>
          var originalPath = parent.originalPath
          var target: IsModule = ModuleTarget(state.circuit.main, parent.module)
          a.target.asPath.foreach { case (instance, _) =>
            originalPath = s"$originalPath.${instance.value}"
            val child = clones.find(_.originalPath == originalPath).getOrElse(
              fail(s"missing target-box clone: $originalPath"))
            target = target.instOf(instance.value, child.module)
          }
          a.copy(target = target)
        }
        record(a, copies.map(_.target.serialize))
        copies
      case a if memoryPolicy(a) =>
        record(a, Seq(a.serialize))
        Seq(a)
      case _: firrtl.transforms.CombinationalPath => Nil // Invalidated path cache, recomputed by Compiler.
      case a @ (MemorySynthInit | MemoryNoSynthInit | firrtl.transforms.NoCircuitDedupAnnotation) => Seq(a)
      case a: firrtl.transforms.NoDedupAnnotation =>
        val copies = moduleClones(a.target.circuit, a.target.module).map(c =>
          a.copy(target = ModuleTarget(state.circuit.main, c.module)))
        record(a, copies.map(_.target.serialize))
        copies
      case a: firrtl.passes.InlineAnnotation =>
        // Keep the pending inline request, with every module/instance target
        // renamed. The bank resolver does not run InlineInstances; consuming
        // this request later requires rebuilding the hierarchy-bound bank map.
        val copies = a.target match {
          case _: CircuitName => Seq(a)
          case m: ModuleName => moduleClones(m.circuit.name, m.name).map(c =>
            a.copy(target = ModuleName(c.module, CircuitName(state.circuit.main))))
          case i: ComponentName => moduleClones(i.module.circuit.name, i.module.name).map(c =>
            a.copy(target = ComponentName(i.name, ModuleName(c.module, CircuitName(state.circuit.main)))))
          case target => fail(s"inline annotation target has no translation contract: ${target.serialize}")
        }
        record(a, copies.map(_.target.serialize))
        copies
      case a: InternalXDCAnnotation =>
        val copies = xdcCopies(a.argumentList).map(args =>
          InternalXDCAnnotation(a.destinationFile, a.formatString, args: _*))
        record(a, copies.map(_.serialize))
        copies
      case a: XDCAnnotation =>
        val copies = xdcCopies(a.argumentList).map(args =>
          XDCAnnotation(a.destinationFile, a.formatString, args: _*))
        record(a, copies.map(_.serialize))
        copies
      case a: FirrtlMemModelAnnotation =>
        val copies = targets(a.target, true).map(t => a.copy(target = t))
        record(a, copies.map(_.target.serialize))
        copies
      case a: DontTouchAnnotation =>
        val copies = targets(a.target, false).map(t => a.copy(target = t))
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a: SynthesisPlusArgConstantAnnotation =>
        if (a.target.circuit != state.circuit.main) fail(s"foreign synthesis constant: ${a.target.serialize}")
        val copies = clones.filter(_.originalModule == a.target.module).map(c =>
          a.copy(target = firrtl.annotations.ModuleTarget(state.circuit.main, c.module)))
        if (copies.isEmpty) fail(s"unreachable synthesis constant: ${a.target.serialize}")
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a: MemoryFileInlineAnnotation =>
        val copies = targets(a.target, true).map { t =>
          if (!initTargets.add(t)) fail(s"overlapping memory initialization annotations: ${t.serialize}")
          a.copy(target = t)
        }
        mappings += JObject(List("source" -> JString(a.serialize),
          "targets" -> JArray(copies.map(c => JString(c.target.serialize)).toList)))
        copies
      case a => fail(s"annotation has no translation contract: ${a.getClass.getName}")
    }
    // Instance-specialized bank wiring is authoritative. Prevent the resolver
    // from merging even stateless repeated helpers and returning stale clone
    // targets or an unowned DedupedResult side channel.
    val translated = (requested :+ firrtl.transforms.NoCircuitDedupAnnotation).distinct
    val provenance = JObject(List("translation_version" -> JInt(translationVersion),
      "source_sha256" -> JString(SemanticStateInventory.sha256(annotationJSON(state.annotations))),
      "translated_sha256" -> JString(SemanticStateInventory.sha256(annotationJSON(translated))),
      "mappings" -> JArray(mappings.toList)))
    (translated, provenance)
  }
}
