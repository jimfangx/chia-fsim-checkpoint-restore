// SPDX-License-Identifier: Apache-2.0
// Adapted from chipsalliance/firrtl v1.6.0 transforms/Dedup.scala.
// Keep FIRRTL 1.6's complete native rename protocol. Only clique hashing adds
// the optional, literal dedup group: unannotated modules form a separate group.
package firrtl.transforms

import firrtl._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.annotations._
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.analyses.InstanceKeyGraph
import firrtl.passes.InferTypes
import firrtl.renamemap.MutableRenameMap
import firrtl.Utils.{throwInternalError, kind, splitRef}
import scala.annotation.tailrec
import scala.collection.mutable

/** Compatibility annotation missing from the pinned FIRRTL 1.6 runtime.
  * The name and wire format match Chisel's actual generated constraints.
  * Generic FIRRTL 1.6 dedup ignores this class; Golden Gate suppresses that
  * scheduler pass and uses the group-aware forced pass instead.
  */
case class DedupGroupAnnotation(target: ModuleTarget, group: String) extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(n: ModuleTarget): DedupGroupAnnotation = copy(target = n)
}

class CheckpointGroupDedupModules(groups: Map[String, String]) extends DedupModules {
  var moduleRenames: Map[String, String] = Map.empty
  var renameHistory: String = ""
  override def execute(state: CircuitState): CircuitState = {
    val result = super.execute(state)
    renameHistory = result.renames.map(_.serialize).getOrElse("")
    result
  }
  override def run(
    c:                  Circuit,
    noDedups:           Seq[String],
    previouslyDupedMap: Map[String, String]
  ): (Circuit, RenameMap, AnnotationSeq) = {

    // RenameMap
    val componentRenameMap = MutableRenameMap()
    componentRenameMap.setCircuit(c.main)

    // Maps module name to corresponding dedup module
    val dedupMap = CheckpointGroupDedupModules.deduplicate(c, noDedups.toSet, previouslyDupedMap, componentRenameMap, groups)
    moduleRenames = dedupMap.map { case (from, to) => from -> to.name }
    val dedupCliques = dedupMap
      .foldLeft(Map.empty[String, Set[String]]) {
        case (dedupCliqueMap, (orig: String, dupMod: DefModule)) =>
          val set = dedupCliqueMap.getOrElse(dupMod.name, Set.empty[String]) + dupMod.name + orig
          dedupCliqueMap + (dupMod.name -> set)
      }
      .flatMap {
        case (dedupName, set) =>
          set.map { _ -> set }
      }

    // Use old module list to preserve ordering
    // Lookup what a module deduped to, if its a duplicate, remove it
    val dedupedModules = {
      val seen = mutable.Set[String]()
      c.modules.flatMap { m =>
        val dedupMod = dedupMap(m.name)
        if (!seen(dedupMod.name)) {
          seen += dedupMod.name
          Some(dedupMod)
        } else {
          None
        }
      }
    }

    val ct = CircuitTarget(c.main)

    val map = dedupMap.map {
      case (from, to) =>
        logger.debug(s"[Dedup] $from -> ${to.name}")
        ct.module(from).asInstanceOf[CompleteTarget] -> Seq(ct.module(to.name))
    }
    val moduleRenameMap = MutableRenameMap()
    moduleRenameMap.recordAll(map)

    // Build instanceify renaming map
    val instanceGraph = InstanceKeyGraph(c)
    val instanceify = MutableRenameMap()
    val moduleName2Index = c.modules
      .map(_.name)
      .zipWithIndex
      .map {
        case (n, i) => {
          c.modules.size match {
            case 0 => (n, 0.0)
            case 1 => (n, 1.0)
            case d => (n, i.toDouble / (d - 1))
          }
        }
      }
      .toMap

    // get the ordered set of instances a module, includes new Deduped modules
    val getChildrenInstances = {
      val childrenMap = instanceGraph.getChildInstances.toMap
      val newModsMap = dedupMap.map {
        case (_, m: Module) =>
          m.name -> InstanceKeyGraph.collectInstances(m)
        case (_, m: DefModule) =>
          m.name -> List()
      }
      (mod: String) => childrenMap.getOrElse(mod, newModsMap(mod))
    }

    val instanceNameMap: Map[OfModule, Map[Instance, Instance]] = {
      dedupMap.map {
        case (oldName, dedupedMod) =>
          val key = OfModule(oldName)
          val value = getChildrenInstances(oldName)
            .zip(getChildrenInstances(dedupedMod.name))
            .map {
              case (oldInst, newInst) => Instance(oldInst.name) -> Instance(newInst.name)
            }
            .toMap
          key -> value
      }.toMap
    }
    val dedupAnnotations = c.modules.map(_.name).map(ct.module).flatMap {
      case mt @ ModuleTarget(c, m) if dedupCliques(m).size > 1 =>
        dedupMap.get(m) match {
          case None => Nil
          case Some(module: DefModule) =>
            val paths = instanceGraph.findInstancesInHierarchy(m)
            // If dedupedAnnos is exactly annos, contains is because dedupedAnnos is type Option
            val newTargets = paths.map { path =>
              val root: IsModule = ct.module(c)
              path.foldLeft(root -> root) {
                case ((oldRelPath, newRelPath), InstanceKeyGraph.InstanceKey(name, mod)) =>
                  if (mod == c) {
                    val mod = CircuitTarget(c).module(c)
                    mod -> mod
                  } else {
                    val enclosingMod = oldRelPath match {
                      case i: InstanceTarget => i.ofModule
                      case m: ModuleTarget   => m.module
                    }
                    val instMap = instanceNameMap(OfModule(enclosingMod))
                    val newInstName = instMap(Instance(name)).value
                    val old = oldRelPath.instOf(name, mod)
                    old -> newRelPath.instOf(newInstName, mod)
                  }
              }
            }

            // Add all relative paths to referredModule to map to new instances
            def addRecord(from: IsMember, to: IsMember): Unit = from match {
              case x: ModuleTarget =>
                instanceify.record(x, to)
              case x: IsComponent =>
                instanceify.record(x, to)
                addRecord(x.stripHierarchy(1), to)
            }
            // Instanceify deduped Modules!
            if (dedupCliques(module.name).size > 1) {
              newTargets.foreach { case (from, to) => addRecord(from, to) }
            }
            // Return Deduped Results
            if (newTargets.size == 1) {
              Seq(DedupedResult(mt, newTargets.headOption.map(_._1), moduleName2Index(m)))
            } else Nil
        }
      case noDedups => Nil
    }

    val finalRenameMap = instanceify.andThen(componentRenameMap).andThen(moduleRenameMap)
    (InferTypes.run(c.copy(modules = dedupedModules)), finalRenameMap, dedupAnnotations.toList)
  }
}

private object CheckpointGroupDedupModules {
  @tailrec
  private def hasBundleType(tpe: Type): Boolean = tpe match {
    case _: BundleType => true
    case _: GroundType => false
    case VectorType(t, _) => hasBundleType(t)
  }

  // Find modules that should not have their ports agnostified to avoid bug in
  // https://github.com/freechipsproject/firrtl/issues/1703
  // Marks modules that have a port of BundleType that are connected via an aggregate connect or
  // partial connect in an instantiating parent
  // Order of modules does not matter
  private def modsToNotAgnostifyPorts(modules: Seq[DefModule]): Set[String] = {
    val dontDedup = mutable.HashSet.empty[String]
    def onModule(mod: DefModule): Unit = {
      val instToModule = mutable.HashMap.empty[String, String]
      def markAggregatePorts(expr: Expression): Unit = {
        if (kind(expr) == InstanceKind && hasBundleType(expr.tpe)) {
          val (WRef(inst, _, _, _), _) = splitRef(expr)
          dontDedup += instToModule(inst)
        }
      }
      def onStmt(stmt: Statement): Unit = {
        stmt.foreach(onStmt)
        stmt match {
          case inst: DefInstance =>
            instToModule(inst.name) = inst.module
          case Connect(_, lhs, rhs) =>
            markAggregatePorts(lhs)
            markAggregatePorts(rhs)
          case PartialConnect(_, lhs, rhs) =>
            markAggregatePorts(lhs)
            markAggregatePorts(rhs)
          case _ =>
        }
      }
      mod.foreach(onStmt)
    }
    modules.foreach(onModule)
    dontDedup.toSet
  }

  /** Visits every module in the circuit, starting at the leaf nodes.
    * Every module is hashed in order to find ones that have the exact
    * same structure and are thus functionally equivalent.
    * Every unique hash is mapped to a human-readable tag which starts with `Dedup#`.
    * @param top CircuitTarget
    * @param moduleLinearization Sequence of modules from leaf to top
    * @param noDedups names of modules that should not be deduped
    * @return A map from tag to names of modules with the same structure and
    *         a RenameMap which maps Module names to their Tag.
    */
  def buildRTLTags(
    top:                 CircuitTarget,
    moduleLinearization: Seq[DefModule],
    noDedups:            Set[String],
    groups:              Map[String, String]
  ): (collection.Map[String, collection.Set[String]], RenameMap) = {
    // maps hash code to human readable tag
    val hashToTag = mutable.HashMap[(Option[String], ir.HashCode), String]()

    // remembers all modules with the same hash
    val hashToNames = mutable.HashMap[(Option[String], ir.HashCode), List[String]]()

    // rename modules that we have already visited to their hash-derived tag name
    val moduleNameToTag = mutable.HashMap[String, String]()

    val dontAgnostifyPorts = modsToNotAgnostifyPorts(moduleLinearization)

    moduleLinearization.foreach { originalModule =>
      val structuralHash = if (noDedups.contains(originalModule.name)) {
        // if we do not want to dedup we just hash the name of the module which is guaranteed to be unique
        StructuralHash.sha256(originalModule.name)
      } else if (dontAgnostifyPorts(originalModule.name)) {
        StructuralHash.sha256WithSignificantPortNames(originalModule, moduleNameToTag)
      } else {
        StructuralHash.sha256(originalModule, moduleNameToTag)
      }

      val hash = groups.get(originalModule.name) -> structuralHash
      if (hashToTag.contains(hash)) {
        hashToNames(hash) = hashToNames(hash) :+ originalModule.name
      } else {
        hashToTag(hash) = "Dedup#" + originalModule.name
        hashToNames(hash) = List(originalModule.name)
      }
      moduleNameToTag(originalModule.name) = hashToTag(hash)
    }

    val tag2all = hashToNames.map { case (hash, names) => hashToTag(hash) -> names.toSet }
    val tagMap = MutableRenameMap()
    moduleNameToTag.foreach { case (name, tag) => tagMap.record(top.module(name), top.module(tag)) }
    (tag2all, tagMap)
  }
  def deduplicate(
    circuit:            Circuit,
    noDedups:           Set[String],
    previousDupResults: Map[String, String],
    renameMap:          MutableRenameMap,
    groups:             Map[String, String]
  ): Map[String, DefModule] = {

    val (moduleMap, moduleLinearization) = {
      val iGraph = InstanceKeyGraph(circuit)
      (iGraph.moduleMap, iGraph.moduleOrder.reverse)
    }
    val main = circuit.main
    val top = CircuitTarget(main)

    // Maps a module name to its agnostic name
    // tagMap is a RenameMap containing ModuleTarget renames of original name to tag name
    // tag2all is a Map of tag to original names of all modules with that tag
    val (tag2all, tagMap) = buildRTLTags(top, moduleLinearization, noDedups, groups)

    // Set tag2name to be the best dedup module name
    val moduleIndex = circuit.modules.zipWithIndex.map { case (m, i) => m.name -> i }.toMap

    // returns the module matching the circuit name or the module with lower index otherwise
    def order(l: String, r: String): String = {
      if (l == main) l
      else if (r == main) r
      else if (moduleIndex(l) < moduleIndex(r)) l
      else r
    }

    // Maps a module's tag to its deduplicated module
    val tag2name = mutable.HashMap.empty[String, String]

    // Maps a deduped module name to its original Module (its instance names need to be updated)
    val moduleMapWithOldNames = tag2all.map {
      case (tag, all: collection.Set[String]) =>
        val dedupWithoutOldName = all.reduce(order)
        val dedupName = previousDupResults.getOrElse(dedupWithoutOldName, dedupWithoutOldName)
        tag2name(tag) = dedupName
        val dedupModule = moduleMap(dedupWithoutOldName) match {
          case e: ExtModule => e.copy(name = dedupName)
          case e: Module    => e.copy(name = dedupName)
        }
        dedupName -> dedupModule
    }.toMap

    // Create map from original to dedup name
    require(tag2name.values.toSeq.distinct.size == tag2name.size,
      "Group dedup unsupported: prior DupedResult names alias distinct structural/group cliques")
    val name2name = moduleMap.keysIterator.map { originalModule =>
      tagMap.get(top.module(originalModule)) match {
        case Some(Seq(Target(_, Some(tag), Nil))) => originalModule -> tag2name(tag)
        case None                                 => originalModule -> originalModule
        case other                                => throwInternalError(other.toString)
      }
    }.toMap

    // Build Remap for modules with deduped module references
    val dedupedName2module = tag2name.map {
      case (tag, name) => name -> DedupModules.dedupInstances(top, name, moduleMapWithOldNames, name2name, renameMap)
    }

    // Build map from original name to corresponding deduped module
    // It is important to flatMap before looking up the DefModules so that they aren't hashed
    val name2module: Map[String, DefModule] =
      tag2all.flatMap { case (tag, names) => names.map(_ -> tag) }
        .mapValues(tag => dedupedName2module(tag2name(tag)))
        .toMap

    // Build renameMap
    val indexedTargets = mutable.HashMap[String, IndexedSeq[ReferenceTarget]]()
    name2module.foreach {
      case (originalName, depModule) =>
        if (originalName != depModule.name) {
          val toSeq = indexedTargets.getOrElseUpdate(depModule.name, DedupModules.computeIndexedNames(circuit.main, depModule))
          val fromSeq = DedupModules.computeIndexedNames(circuit.main, moduleMap(originalName))
          DedupModules.computeRenameMap(fromSeq, toSeq, renameMap)
        }
    }

    name2module
  }

}
