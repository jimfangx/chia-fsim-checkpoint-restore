// See LICENSE for license details.
package midas.checkpoint

import org.json4s._

/** Static selector allocation for the synthesized region router. Each region
  * retains its endpoint-local selectors; bank 255 remains the physical CE bank.
  * No all-owner freeze or complete simulator coverage follows from allocation.
  */
object CheckpointRegionRouting {
  case class Region(bankBase: Int, bankCount: Int, ceBase: Int, ceCount: Int)
  case class Result(regions: Seq[Region], entries: Seq[StateAccessEntry], binding: String) {
    def emit: Seq[midas.stage.GoldenGateOutputFileAnnotation] = Seq(
      midas.stage.GoldenGateOutputFileAnnotation(routerSource, ".checkpoint-region-router.sv"),
      midas.stage.GoldenGateOutputFileAnnotation(parameters, ".checkpoint-region-router-parameters.svh"),
      midas.stage.GoldenGateOutputFileAnnotation(binding, ".checkpoint-region-routing.json"))
    private def packed(width: Int, values: Seq[Int]): String =
      s"${width * values.size}'h" + values.reverse.map(v =>
        ("0" * (width / 4) + v.toHexString).takeRight(width / 4)).mkString
    def parameters: String = Seq("REGIONS" -> regions.size.toString,
      "BANK_BASE" -> packed(8, regions.map(_.bankBase)),
      "BANK_COUNT" -> packed(8, regions.map(_.bankCount)),
      "CE_BASE" -> packed(16, regions.map(_.ceBase)),
      "CE_COUNT" -> packed(16, regions.map(_.ceCount))).map {
      case (k, v) => s".$k($v)"
    }.mkString(", ")
  }
  private def routerSource: String = {
    val resource = scala.io.Source.fromResource("CheckpointRegionStateRouter.sv")
    try resource.mkString finally resource.close()
  }
  def allocate(endpoints: Seq[Seq[StateAccessEntry]]): Result = {
    def check(ok: Boolean, why: String): Unit = require(ok, "checkpoint region routing: " + why)
    check(endpoints.nonEmpty && endpoints.size <= 255, "unsupported region count")
    check(endpoints.forall(_.nonEmpty) && endpoints.map(_.size.toLong).sum <= 65535,
      "empty region or descriptor count exceeds ABI")
    val all = endpoints.flatten
    check(all.map(_.stateID).distinct.size == all.size, "overlapping or duplicate state owners")
    var nextBank = 0
    var nextCE = 0
    val regions = endpoints.map { entries =>
      check(entries.map(e => e.bank -> e.slot).distinct.size == entries.size, "duplicate endpoint selector")
      val functional = entries.filterNot(_.bank == 255)
      val physical = entries.filter(_.bank == 255)
      check(functional.nonEmpty, "region needs functional storage")
      val banks = functional.map(_.bank).max + 1
      check(functional.map(_.bank).distinct.sorted == (0 until banks), "noncontiguous endpoint banks")
      check(physical.map(_.slot) == physical.indices && physical.forall(e => e.width == 1 && e.depth == 1),
        "physical CE selectors must be ordered unit-width words")
      val region = Region(nextBank, banks, nextCE, physical.size)
      nextBank += banks; nextCE += physical.size
      check(nextBank <= 255 && nextCE <= 65535, "selector allocation exceeds transport ABI")
      region
    }
    val mapped = endpoints.zip(regions).map { case (entries, region) => entries.map { e =>
      if (e.bank == 255) e.copy(slot = region.ceBase + e.slot)
      else e.copy(bank = region.bankBase + e.bank)
    }}
    // The physical controller's sample vector is concatenated in region order.
    val entries = mapped.flatten.filterNot(_.bank == 255) ++ mapped.flatten.filter(_.bank == 255)
    val binding = SemanticStateInventory.canonical(JObject(List(
      "routing_version" -> JInt(1), "scope" -> JString("native_region_selector_routing_only"),
      "router_sha256" -> JString(SemanticStateInventory.sha256(routerSource)),
      "clock_contract" -> JString("shared_raw_clock_and_reset_all_endpoints"),
      "freeze_contract" -> JString("external_common_low_phase_all_owner_cut_required"),
      "complete_fpga_capture" -> JBool(false), "deployable_capture_image" -> JBool(false),
      "regions" -> JArray(regions.zipWithIndex.map { case (r, i) => JObject(List(
        "index" -> JInt(i), "bank_base" -> JInt(r.bankBase), "bank_count" -> JInt(r.bankCount),
        "ce_base" -> JInt(r.ceBase), "ce_count" -> JInt(r.ceCount))) }.toList))))
    Result(regions, entries, binding)
  }
}
