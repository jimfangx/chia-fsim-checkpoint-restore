// See LICENSE for license details.
package midas.stage

import firrtl.{AnnotationSeq, CircuitState, Transform}
import firrtl.annotations.{JsonProtocol, NoTargetAnnotation}
import firrtl.stage.RunFirrtlTransformAnnotation
import midas.checkpoint.SemanticStateInventory.{canonical, sha256}
import org.json4s._
import org.json4s.native.JsonMethods.parse

/** Historical request resolved by the stage's fixed SystemVerilog policy.
  * This is never a request to run the original emitter again.
  */
case class GoldenGateEmitterPolicyAnnotation(requestJSON: String, policy: String) extends NoTargetAnnotation

object GoldenGateEmitterPolicy {
  private val policy = "golden-gate-system-verilog-v1"
  private def defaultRequest = RunFirrtlTransformAnnotation(new firrtl.VerilogEmitter)
  private def requestJSON = JsonProtocol.serialize(Seq(defaultRequest))
  private def fail(why: String): Nothing =
    throw new IllegalArgumentException(s"Golden Gate emitter policy: $why")

  /** AddDefaults in the runtime FIRRTL 1.6 jar supplies this exact request.
    * Reject subclasses, other passes and duplicate requests before Compiler
    * can canonicalize them. Golden Gate owns the final emitter selection.
    */
  private[midas] def consumeEmitterRequest(annotations: AnnotationSeq): AnnotationSeq = {
    if (annotations.exists(_.isInstanceOf[GoldenGateEmitterPolicyAnnotation])) fail("request already consumed")
    val requests = annotations.collect { case a: RunFirrtlTransformAnnotation => a }
    if (requests.size != 1) fail("exactly one default emitter request is required")
    if (requests.head.transform.getClass != classOf[firrtl.VerilogEmitter])
      fail(s"unsupported transform request: ${requests.head.transform.getClass.getName}")
    val receipt = GoldenGateEmitterPolicyAnnotation(JsonProtocol.serialize(requests), policy)
    validate(receipt)
    annotations.map {
      case _: RunFirrtlTransformAnnotation => receipt
      case a => a
    }
  }

  def validate(a: GoldenGateEmitterPolicyAnnotation): Unit = {
    if (a.policy != policy || a.requestJSON != requestJSON) fail("invalid consumed default emitter request")
  }

  /** Complete context row, including the original serialized request. */
  def contextJSON(a: GoldenGateEmitterPolicyAnnotation): String = {
    validate(a)
    canonical(JObject(List(
      "producer" -> JString("midas.stage.GoldenGateCompilerPhase"),
      "policy" -> JString(a.policy), "request" -> parse(a.requestJSON),
      "request_sha256" -> JString(sha256(a.requestJSON)),
      "output_language" -> JString("SystemVerilog"),
      "plain_emitter" -> JString(classOf[firrtl.SystemVerilogEmitter].getName),
      "observed_emitter" -> JString(classOf[midas.checkpoint.SemanticCleanEmitter].getName))))
  }

  private def receipts(annotations: AnnotationSeq): Seq[GoldenGateEmitterPolicyAnnotation] = {
    // Subcircuit elaboration can introduce genuine requests such as
    // InlineInstances. Those remain live for their own consumers and fail the
    // bank's annotation preflight until they have an explicit contract. This
    // policy only owns emitter requests; it must never consume another pass.
    val pendingEmitters = annotations.collect {
      case a: RunFirrtlTransformAnnotation if a.transform.isInstanceOf[firrtl.Emitter] =>
        a.transform.getClass.getName
    }
    if (pendingEmitters.nonEmpty) fail("unconsumed emitter request: " + pendingEmitters.mkString(", "))
    val values = annotations.collect { case a: GoldenGateEmitterPolicyAnnotation => a }
    if (values.size > 1) fail("duplicate consumed emitter request")
    values.foreach(validate)
    values
  }

  private[midas] def validatePreserved(expected: AnnotationSeq, actual: AnnotationSeq): Unit = {
    if (receipts(expected) != receipts(actual)) fail("consumed emitter request changed or missing")
  }

  /** Bind the actual selected emitter and its actual sole SystemVerilog result.
    * Receipt-free internal backend callers remain supported, but stage entry
    * always consumes one request and checks its survival through the pipeline.
    */
  private[midas] def emissionBinding(expected: AnnotationSeq, emitted: CircuitState,
      emitter: Transform, observed: Boolean): Seq[GoldenGateOutputFileAnnotation] = {
    validatePreserved(expected, emitted.annotations)
    val history = receipts(expected)
    if (history.isEmpty) return Nil
    val selected = if (observed) classOf[midas.checkpoint.SemanticCleanEmitter] else classOf[firrtl.SystemVerilogEmitter]
    if (emitter.getClass != selected) fail("actual emitter does not implement the selected policy")
    val results = emitted.annotations.collect { case a: firrtl.EmittedCircuitAnnotation[_] => a }
    if (results.size != 1) fail("exactly one emitted SystemVerilog circuit is required")
    val result = results.head match {
      case firrtl.EmittedVerilogCircuitAnnotation(v) if v.outputSuffix == ".sv" &&
          v.name == emitted.circuit.main && v.value.nonEmpty => v
      case _ => fail("actual emitted circuit does not implement the SystemVerilog policy")
    }
    Seq(GoldenGateOutputFileAnnotation(canonical(JObject(List(
      "binding_version" -> JInt(1), "scope" -> JString("compiler_emitter_policy_only"),
      "deployable_capture_image" -> JBool(false),
      "consumed_policy" -> parse(contextJSON(history.head)),
      "consumed_policy_sha256" -> JString(sha256(contextJSON(history.head))),
      "observed_emission" -> JBool(observed), "actual_emitter" -> JString(emitter.getClass.getName),
      "emitted_circuit" -> JString(result.name), "output_suffix" -> JString(result.outputSuffix),
      "emitted_verilog_sha256" -> JString(sha256(result.value))))), ".golden-gate-emitter.json"))
  }
}
