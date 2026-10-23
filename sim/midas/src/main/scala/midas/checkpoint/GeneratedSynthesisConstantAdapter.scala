// See LICENSE for license details.
package midas.checkpoint

import firrtl.CircuitState
import firrtl.annotations.{ModuleTarget, SingleTargetAnnotation}
import firrtl.ir._
import firrtl.transforms.BlackBoxInlineAnno

/** Explicitly records the SYNTHESIS branch of the exact Rocket plusarg model.
  * This is a combinational constant, never the metasim model's myplus owner.
  * Keep this annotation through instance specialization and descriptor checks.
  */
case class SynthesisPlusArgConstantAnnotation(target: ModuleTarget, sourceSHA256: String,
    originalCircuitSHA256: String, width: BigInt, default: BigInt, format: String)
    extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(n: ModuleTarget): SynthesisPlusArgConstantAnnotation = copy(target = n)
}

object GeneratedSynthesisConstantAdapter {
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Generated checkpoint synthesis constant unsupported: $why")

  /** Opt-in only. Call before inventory, and keep the returned annotations.
    * Never use this on a metasim donor: it deliberately selects FPGA semantics.
    */
  def prepare(input: CircuitState): CircuitState = {
    if (input.annotations.exists(_.isInstanceOf[SynthesisPlusArgConstantAnnotation]))
      fail("already prepared synthesis constants")
    val sourceCircuit = SemanticStateInventory.sha256(input.circuit.serialize)
    val annotations = scala.collection.mutable.ArrayBuffer.empty[SynthesisPlusArgConstantAnnotation]
    val replaced = input.circuit.modules.map {
      case m: ExtModule if m.defname == "plusarg_reader" =>
        val sources = input.annotations.collect { case a: BlackBoxInlineAnno if a.target.name == m.name => a }
        val width = m.params.collect { case IntParam("WIDTH", w) => w }
        val default = m.params.collect { case IntParam("DEFAULT", v) => v }
        val format = m.params.collect { case StringParam("FORMAT", s) => s.string }
        if (sources.size != 1 || sources.head.target.circuit.name != input.circuit.main ||
            SemanticStateInventory.sha256(sources.head.text) != SimulatorStateInventory.plusargModelSHA256 ||
            m.params.size != 3 || width.size != 1 || default.size != 1 || format.size != 1 ||
            width.head <= 0 || width.head > Int.MaxValue || default.head < 0 ||
            default.head.bitLength > width.head || !format.head.matches("[A-Za-z_][A-Za-z0-9_]*=%[dbh]") ||
            m.ports.map(p => (p.name, p.direction, p.tpe)) !=
              Seq(("out", Output, UIntType(IntWidth(width.head)))))
          fail(s"no exact SYNTHESIS plusarg implementation for ${m.name}")
        annotations += SynthesisPlusArgConstantAnnotation(ModuleTarget(input.circuit.main, m.name),
          SimulatorStateInventory.plusargModelSHA256, sourceCircuit, width.head, default.head, format.head)
        Module(m.info, m.name, m.ports, Connect(NoInfo,
          firrtl.WRef("out", UIntType(IntWidth(width.head))), UIntLiteral(default.head, IntWidth(width.head))))
      case m => m
    }
    val names = annotations.map(_.target.module).toSet
    val result = input.copy(circuit = input.circuit.copy(modules = replaced),
      annotations = input.annotations.filterNot {
        case a: BlackBoxInlineAnno if names(a.target.name) => true
        case _ => false
      } ++ annotations)
    validate(result, instrumented = false)
    result
  }

  /** Reconstruct the concrete implementation, rather than trusting metadata.
    * Instrumentation may add only its checked ckpt_ interface and read muxes;
    * a synthesis constant has no functional storage, children or clock pins.
    */
  def validate(state: CircuitState, instrumented: Boolean): Unit = {
    val annotations = state.annotations.collect { case a: SynthesisPlusArgConstantAnnotation => a }
    if (annotations.map(_.target).distinct.size != annotations.size) fail("duplicate constant annotation")
    annotations.foreach { a =>
      if (a.target.circuit != state.circuit.main || a.sourceSHA256 != SimulatorStateInventory.plusargModelSHA256 ||
          !a.originalCircuitSHA256.matches("[0-9a-f]{64}") || a.width <= 0 || a.width > Int.MaxValue ||
          a.default < 0 || a.default.bitLength > a.width || !a.format.matches("[A-Za-z_][A-Za-z0-9_]*=%[dbh]"))
        fail(s"altered synthesis provenance at ${a.target.serialize}")
      val m = state.circuit.modules.collectFirst { case m: Module if m.name == a.target.module => m }
        .getOrElse(fail(s"missing concrete constant at ${a.target.serialize}"))
      val all = scala.collection.mutable.ArrayBuffer.empty[Statement]
      def visit(s: Statement): Unit = { all += s; s.foreachStmt(visit) }; visit(m.body)
      val ports = if (instrumented) m.ports.filterNot(_.name.startsWith("ckpt_")) else m.ports
      val output = all.collect { case c: Connect if c.loc.serialize == "out" => c.expr }
      if (ports.map(p => (p.name, p.direction, p.tpe)) != Seq(("out", Output, UIntType(IntWidth(a.width)))) ||
          output.toSeq != Seq(UIntLiteral(a.default, IntWidth(a.width))) ||
          all.exists {
            case _: DefRegister | _: DefMemory | _: firrtl.WDefInstance | _: Print | _: Stop | _: Verification | _: Attach => true
            case c: Connect if c.loc.serialize != "out" => !instrumented || !c.loc.serialize.startsWith("ckpt_")
            case _: DefNode | _: DefWire => true
            case _ => false
          }) fail(s"altered synthesized constant at ${a.target.serialize}")
    }
  }
}
