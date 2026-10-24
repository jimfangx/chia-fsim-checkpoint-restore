// See LICENSE for license details.

package midas
package passes

import firrtl._
import firrtl.annotations.{CircuitName, InstanceTarget, ModuleTarget}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler
import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils.module_type
import midas.passes.Utils._
import freechips.rocketchip.diplomacy.LazyModule

import midas.core._
import midas.platform.PlatformShim
import midas.stage.{GoldenGateOutputFileAnnotation, OutputFileBuilder}

private[passes] class SimulationMapping(targetName: String) extends firrtl.Transform {
  def inputForm     = LowForm
  def outputForm    = HighForm
  override def name = "[Golden Gate] Simulation Mapping"

  private def generateHeaderAnnos(c: PlatformShim): Seq[GoldenGateOutputFileAnnotation] = {
    val csb = new OutputFileBuilder(
      """// Golden Gate-generated Driver Header
        |// This contains target-specific preprocessor macro definitions,
        |// and encodes all required bridge metadata to instantiate bridge drivers.
        |""".stripMargin,
      fileSuffix = ".const.h",
    )
    c.genHeader(csb.getBuilder, targetName)

    val vsb = new OutputFileBuilder(
      """// Golden Gate-generated Verilog Header
        |// This file encodes variable width fields used in MIDAS-level simulation
        |// and is not used in FPGA compilation flows.
        |""".stripMargin,
      fileSuffix = ".const.vh",
    )

    val psb = new OutputFileBuilder(
      """// Golden Gate-generated Partition Switch Header
        |// This file encodes partition interface widths.
        |""".stripMargin,
      fileSuffix = ".partition.const.h",
    )
    c.genPartitioningHeader(psb.getBuilder)

    // Golden Gate-generated Peer to Peer addr map
    // This file encodes AXI4 address ranges for bridges
    // that expects to receive PCIS transactions from a peer FPGA
    // that sends requests out on PCIM.
    val p2psb = new OutputFileBuilder("", fileSuffix = ".peer2peer.const.yaml")
    c.genPeerToPeerAddrMap(p2psb.getBuilder)

    vsb.append("`ifndef __%s_H\n".format(targetName.toUpperCase))
    vsb.append("`define __%s_H\n".format(targetName.toUpperCase))
    c.genVHeader(vsb.getBuilder)
    vsb.append("`endif  // __%s_H\n".format(targetName.toUpperCase))
    Seq(csb.toAnnotation, vsb.toAnnotation, psb.toAnnotation, p2psb.toAnnotation)
  }

  def execute(innerState: CircuitState) = {
    // Generate a port map to look up the types of the IO of the channels
    implicit val p  =
      innerState.annotations.collectFirst({ case midas.stage.phases.ConfigParametersAnnotation(p) => p }).get
    val circuit     = innerState.circuit
    val portTypeMap = circuit.modules
      .filter(_.name == circuit.main)
      .flatMap({ m =>
        val mTarget = ModuleTarget(circuit.main, m.name)
        m.ports.map({ p => mTarget.ref(p.name) -> p })
      })
      .toMap

    // Lower the inner circuit in preparation for linking
    // This prevents having to worry about matching aggregate structure in the wrapper IO
    val loweredInnerState = new Compiler(Forms.LowForm, Forms.HighForm).execute(innerState)
    val innerCircuit      = loweredInnerState.circuit

    // Generate the encapsulating simulator RTL
    lazy val shim                  = PlatformShim(SimWrapperConfig(innerState.annotations, portTypeMap))
    val (chirrtl, elaboratedAnnos) = midas.ElaborateChiselSubCircuit(LazyModule(shim).module)

    val outerAnnos = PreLinkRenamingAnnotation(Namespace(innerCircuit)) +: elaboratedAnnos
    val outerState = new Compiler(Forms.LowForm ++ Seq(Dependency(PreLinkRenaming)))
      .execute(CircuitState(chirrtl, ChirrtlForm, outerAnnos))

    val linkedState = SimulationMapping.link(loweredInnerState, outerState)
    writeState(linkedState, "post-sim-mapping.fir")
    linkedState.copy(annotations = linkedState.annotations ++ generateHeaderAnnos(shim))
  }
}

private[midas] object SimulationMapping {
  // Link the exact annotated placeholder, never a guessed instance/module name.
  // Return native renames with the circuit so *all* target-bearing annotations
  // follow the replacement when FIRRTL applies the transform's rename map.
  private[midas] def link(innerState: CircuitState, outerState: CircuitState): CircuitState = {
    val inner = innerState.circuit
    val outer = outerState.circuit
    def check(ok: Boolean, why: String): Unit = require(ok, s"SimulationMapping: $why")
    check(inner.modules.map(_.name).distinct.size == inner.modules.size &&
      outer.modules.map(_.name).distinct.size == outer.modules.size, "duplicate module definitions")
    check((inner.modules.map(_.name).toSet intersect outer.modules.map(_.name).toSet).isEmpty,
      "pre-link module namespaces overlap")
    val boundaries = outerState.annotations.collect { case a: TargetBoxAnnotation => a.target }
    check(boundaries.size == 1, "requires exactly one TargetBoxAnnotation")
    val boundary = boundaries.head match {
      case t: InstanceTarget => t
      case _ => throw new IllegalArgumentException("SimulationMapping: target boundary must be an instance")
    }
    check(boundary.circuit == outer.main && boundary.module == outer.main,
      "target boundary must be rooted at the outer circuit top")
    val definitions = outer.modules.map(m => m.name -> m).toMap
    def instances(m: DefModule): Seq[WDefInstance] = {
      val found = scala.collection.mutable.ArrayBuffer.empty[WDefInstance]
      def visit(s: Statement): Unit = {
        s match { case i: WDefInstance => found += i; case _ => () }
        s.foreachStmt(visit)
      }
      m match { case m: Module => visit(m.body); case _ => () }
      found.toSeq
    }
    var owner = outer.main
    boundary.asPath.foreach { case (instance, child) =>
      val matches = definitions.get(owner).toSeq.flatMap(instances).filter(_.name == instance.value)
      check(matches.size == 1 && matches.head.module == child.value,
        s"target boundary instance path mismatch: ${boundary.serialize}")
      owner = child.value
    }
    val placeholder = definitions.get(owner) match {
      case Some(m: ExtModule) => m
      case _ => throw new IllegalArgumentException("SimulationMapping: target placeholder must be an external module")
    }
    check(outer.modules.flatMap(instances).count(_.module == placeholder.name) == 1,
      "target placeholder must have exactly one instance")
    def countConcrete(name: String, ancestors: Set[String]): Int = {
      check(!ancestors(name), "recursive outer hierarchy")
      check(definitions.contains(name), s"missing outer module: $name")
      if (name == placeholder.name) 1
      else instances(definitions(name)).map(i => countConcrete(i.module, ancestors + name)).sum
    }
    check(countConcrete(outer.main, Set.empty) == 1,
      "target placeholder must have exactly one concrete instance")
    val target = inner.modules.collectFirst { case m: Module if m.name == inner.main => m }
      .getOrElse(throw new IllegalArgumentException("SimulationMapping: inner top must be a concrete module"))
    val interface = (m: DefModule) => m.ports.map(p => (p.name, p.direction, p.tpe))
    check(interface(placeholder).toSet == interface(target).toSet,
      "target placeholder and inner top interfaces differ")
    val modules = outer.modules.filterNot(_.name == placeholder.name).map {
      case m: Module if m.name == boundary.encapsulatingModule =>
        m.copy(info = inner.info, body = initStmt(inner.main, boundary.instance, module_type(target))(m.body))
      case m => m
    } ++ inner.modules
    val renames = RenameMap(Map(CircuitName(inner.main) -> Seq(CircuitName(outer.main))))
    renames.record(ModuleTarget(outer.main, placeholder.name), ModuleTarget(outer.main, inner.main))
    val innerAnnos = innerState.annotations.filterNot(_.isInstanceOf[midas.targetutils.FAMEAnnotation])
    CircuitState(Circuit(outer.info, modules, outer.main), HighForm,
      innerAnnos ++ outerState.annotations, Some(renames))
  }

  // Note: this only runs on the validated SimulationWrapper Module.
  private def initStmt(targetModuleName: String, targetInstName: String, targetType: Type)(s: Statement): Statement =
    s match {
      case s @ WDefInstance(_, name, _, _) if name == targetInstName =>
        Block(
          Seq(
            s.copy(module = targetModuleName, tpe = targetType),
            IsInvalid(NoInfo, WRef(name)),
          )
        )
      case s                                                         => s.map(initStmt(targetModuleName, targetInstName, targetType))
    }
}
