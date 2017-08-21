package com.phoenixkahlo.hellcraft.util.fields

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

@CarboniteWith(classOf[FieldNode])
case class FractionField(bytes: ByteField) {

  def size: V3I = bytes.size

  def apply(v: V3I): Option[Float] =
    bytes(v).map(b => (b & 0xFF) / 255f)

  def updated(v: V3I, n: Float): FractionField =
    FractionField(bytes.updated(v, (n * 255).toByte))


  override def toString: String =
    "FractionField(" + Origin.until(size).map(apply(_).get) + ")"

}

object FractionField {

  def apply(size: V3I, gen: V3I => Float): FractionField = {
    FractionField(ByteField(size, v => (gen(v) * 255).toByte))
  }

}