package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.graphics.{DirtTID, ErrorTID, SheetTextureID, StoneTID}

sealed trait Material {
  def id: Byte
  def tid: SheetTextureID
}

case object Air extends Material {
  override val id: Byte = 0
  override def tid: SheetTextureID = ErrorTID
}

case object Stone extends Material {
  override val id: Byte = 1
  override val tid: SheetTextureID = StoneTID
}

case object Dirt extends Material {
  override val id: Byte = 2
  override val tid: SheetTextureID = DirtTID
}

object Materials {
  val seq = Seq(Air, Stone, Dirt)

  val map = seq map (m => m.id -> m) toMap

  def apply(b: Byte): Material = map(b)
}