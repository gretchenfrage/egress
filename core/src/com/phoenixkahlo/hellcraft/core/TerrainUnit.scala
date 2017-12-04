package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.core.Materials.seq
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.util.fields.IDMapping

sealed trait TerrainUnit extends Serializable {
  def id: Byte
  def tid: SheetTextureID
}

case object Air extends Material {
  override val id: Byte = 0
  override val tid: SheetTextureID = ErrorTID
}

sealed trait Material extends TerrainUnit
sealed trait Block extends TerrainUnit

object Materials {
  case object Stone extends Material {
    override val id: Byte = 1
    override val tid: SheetTextureID = StoneTID
  }
  case object Dirt extends Material {
    override val id: Byte = 2
    override val tid: SheetTextureID = DirtTID
  }
  case object Grass extends Material {
    override def id: Byte = 3
    override def tid: SheetTextureID = GrassTID
  }
  val seq = Seq(Stone, Dirt, Grass)
}

object Blocks {
  case object Stone extends Block {
    override def id: Byte = -1
    override def tid: SheetTextureID = StoneTID
  }
  case object Dirt extends Block {
    override def id: Byte = -2
    override def tid: SheetTextureID = DirtTID
  }
  case object Brick extends Block {
    override def id: Byte = -3
    override def tid: SheetTextureID = BrickTID
  }
  val seq = Seq(Stone, Dirt, Brick)
}

object TerrainUnits extends IDMapping[TerrainUnit] {
  val seq: Seq[TerrainUnit] = Air +: Materials.seq ++: Blocks.seq
  val map: Map[Byte, TerrainUnit] = seq map (m => m.id -> m) toMap
  def apply(b: Byte): TerrainUnit = map(b)
  override def id(item: TerrainUnit): Byte = item.id
  override def lookup(id: Byte): TerrainUnit = map(id)
}