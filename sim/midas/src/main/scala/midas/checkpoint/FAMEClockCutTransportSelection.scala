// See LICENSE for license details.
package midas.checkpoint

import firrtl.{AnnotationSeq, CircuitState}
import firrtl.annotations.{IsModule, ModuleTarget, NoTargetAnnotation}
import firrtl.ir.ExtModule
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** An explicit compiler request, never a freeze-owner acknowledgement. */
case class FAMEClockCutTransportSelectionAnnotation(primaryDomain: String, fameOwner: String)
    extends NoTargetAnnotation with Unserializable
object FAMEClockCutTransportSelectionAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "simulator-existing-transport-fame-witness",
    toAnnotationSeq = value => {
      val parts = value.split(":", 2)
      require(parts.length == 2, "existing-transport FAME witness requires PRIMARY_DOMAIN:FAME_OWNER")
      Seq(FAMEClockCutTransportSelectionAnnotation(parts(0), parts(1)))
    },
    helpText = "Qualify the actual reserved widget transport with a FAME witness; independent root owner inputs and the incomplete-image guard remain required.",
  ))
}

/** Immutable history of compiler selection; it grants no image admission. */
case class FAMEClockCutTransportSelectionBindingAnnotation(body: String) extends NoTargetAnnotation

/** Select the ONE registered widget's transport from its authenticated native
  * inline pair. No instance name, owner ordering or second collector is supplied
  * by the caller. The existing strict root-input provenance checks still apply.
  */
object FAMEClockCutTransportSelection {
  import SemanticStateInventory.{canonical, sha256}
  private def check(ok: Boolean, why: String): Unit =
    require(ok, "compiler FAME transport selection: " + why)

  def requested(as: AnnotationSeq): Boolean = as.exists(_.isInstanceOf[FAMEClockCutTransportSelectionAnnotation])
  def validateCardinality(as: AnnotationSeq): Unit = {
    val requests = as.collect { case a: FAMEClockCutTransportSelectionAnnotation => a }
    check(!as.exists(_.isInstanceOf[FAMEClockCutTransportSelectionBindingAnnotation]), "selection already consumed")
    if (requests.isEmpty) return
    check(requests.size == 1 && requests.head.primaryDomain.matches("[A-Za-z_][A-Za-z0-9_$]*") &&
      requests.head.fameOwner.matches("[A-Za-z0-9_.:/-]+"), "one explicit primary domain and owner required")
    check(!NativeCheckpointRegionCompilerProbe.requested(as), "regional graft and transport selection require separate compiler invocations")
  }

  def validatePreserved(before: AnnotationSeq, after: AnnotationSeq): Unit = {
    validateCardinality(before); validateCardinality(after)
    check(before.collect { case a: FAMEClockCutTransportSelectionAnnotation => a } ==
      after.collect { case a: FAMEClockCutTransportSelectionAnnotation => a }, "request changed or lost before selection")
  }

  def apply(before: CircuitState, after: CircuitState): CircuitState = {
    validatePreserved(before.annotations, after.annotations)
    val requests = before.annotations.collect { case a: FAMEClockCutTransportSelectionAnnotation => a }
    if (requests.isEmpty) return after
    val request = requests.head
    val execution = before.annotations.collect { case a: NativeCheckpointWidgetExecutionAnnotation => a }
    check(execution.size == 1 && execution.head.target.circuit == before.circuit.main &&
      execution.head.target.module == before.circuit.main, "one rooted actual widget execution required")
    check(execution.head.owners.contains(request.fameOwner), "selected FAME owner absent from actual widget ordering")
    // This proves actual widget registration, every hierarchy hop, the original
    // resources/ABI, peer routes, and the exact compiler-produced inline history.
    val peerText = NativeCheckpointWidgetPeerRouting.inspect(before, after)
    val peer = parse(peerText)
    val JString(path) = peer \ "transport"
    val live = GeneratedExternalOwners.instances(after, after.circuit.main).toMap
    val parts = path.split('.').toSeq
    check(parts.head == after.circuit.main && parts.size > 1, "transport outside actual root")
    var target: IsModule = ModuleTarget(after.circuit.main, after.circuit.main)
    parts.indices.drop(1).foreach { index =>
      val definition = live.getOrElse(parts.take(index + 1).mkString("."),
        throw new IllegalArgumentException("compiler FAME transport selection: stale transport hierarchy"))
      target = target.instOf(parts(index), definition.name)
    }
    check(live(path).isInstanceOf[ExtModule], "actual transport must remain external")
    val rooted = target.asInstanceOf[firrtl.annotations.InstanceTarget]
    val wired = FAMEClockCutTransportWiring.connect(after, rooted, request.primaryDomain,
      execution.head.owners, request.fameOwner)
    val binding = canonical(JObject(List(
      "selection_version" -> JInt(1), "scope" -> JString("actual_compiler_registered_widget_fame_witness_only"),
      "primary_domain" -> JString(request.primaryDomain), "fame_owner" -> JString(request.fameOwner),
      "widget_execution" -> JString(execution.head.target.serialize),
      "peer_routing_sha256" -> JString(sha256(peerText)), "transport_binding" -> parse(wired.binding),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "production_freeze_owners_bound" -> JBool(false), "metasim_restore_supported" -> JBool(false))))
    wired.state.copy(annotations = wired.state.annotations.filterNot(_.isInstanceOf[FAMEClockCutTransportSelectionAnnotation]) :+
      FAMEClockCutTransportSelectionBindingAnnotation(binding))
  }

  def validate(before: CircuitState, after: CircuitState, result: CircuitState): Unit = {
    val expected = apply(before, after)
    check(expected.circuit == result.circuit && expected.form == result.form &&
      expected.annotations.toVector == result.annotations.toVector &&
      expected.renames.map(_.serialize.linesIterator.toSeq.sorted) ==
        result.renames.map(_.serialize.linesIterator.toSeq.sorted), "selection differs from independent regeneration")
  }
}
