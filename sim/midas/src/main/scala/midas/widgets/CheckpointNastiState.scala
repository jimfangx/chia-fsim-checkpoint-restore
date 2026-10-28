// See LICENSE for license details.
package midas.widgets

import chisel3._
import chisel3.util.Cat

/** Canonical observation of every original adapter register. Address and ID
  * slots are zero extended to 64 bits, independently of the platform geometry.
  * This live combinational vector neither freezes nor restores its source.
  * The enclosing widget's single-edge shadow supplies a coherent copy.
  */
object CheckpointNastiState {
  val addressFields = Seq("addr" -> 64, "len" -> 8, "size" -> 3,
    "burst" -> 2, "lock" -> 1, "cache" -> 4, "prot" -> 3,
    "qos" -> 4, "region" -> 4, "id" -> 64, "user" -> 1)
  val dataFields = Seq("data" -> 32, "last" -> 1, "id" -> 64,
    "strb" -> 4, "user" -> 1)
  val scalarFields = Seq("haveAW" -> 1, "haveW" -> 1, "haveAR" -> 1,
    "badW" -> 1, "busy" -> 1, "writing" -> 1, "responseValid" -> 1,
    "responseError" -> 1, "responseData" -> 32, "responseId" -> 64,
    "errorReadRemaining" -> 8)
  val fields = Seq("aw" -> addressFields, "wd" -> dataFields,
    "ar" -> addressFields).flatMap { case (prefix, fs) =>
      fs.map { case (name, width) => (prefix + "_" + name, width) }
    } ++ scalarFields
  val bits = fields.map(_._2).sum

  def channel(prefix: String, value: Record, layout: Seq[(String, Int)]): Seq[(String, UInt)] = {
    require(value.elements.keySet == layout.map(_._1).toSet,
      "checkpoint NASTI state: changed channel fields: " + prefix)
    layout.map { case (name, _) => (prefix + "_" + name, value.elements(name).asUInt) }
  }

  def pack(values: Seq[(String, UInt)]): UInt = {
    require(values.map(_._1) == fields.map(_._1), "checkpoint NASTI state: changed register schema")
    Cat(values.zip(fields).reverse.map { case ((name, value), (_, width)) =>
      require(value.getWidth > 0 && value.getWidth <= width,
        "checkpoint NASTI state: unsupported register width: " + name)
      require(name.endsWith("_addr") || name.endsWith("_id") || name == "responseId" ||
        value.getWidth == width, "checkpoint NASTI state: changed register width: " + name)
      value.pad(width)
    })
  }
}
