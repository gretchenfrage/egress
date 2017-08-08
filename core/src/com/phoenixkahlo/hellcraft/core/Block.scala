package com.phoenixkahlo.hellcraft.core

import scala.collection.immutable.Map

/**
  * The block singletons.
  */
sealed abstract class Block(
                             val id: Byte,
                             val tid: SheetTextureID,
                             val isOpaque: Boolean = true,
                             val isCorporeal: Boolean = true
                    ) {

  def isTranslucent = !isOpaque

  def isIncorporeal = !isCorporeal

}

case object Air extends Block(0, null, isOpaque = false, isCorporeal = false)
case object Stone extends Block(1, StoneTID)
case object Dirt extends Block(2, DirtTID)
case object Brick extends Block(3, BrickTID)
case object Grass extends Block(4, GrassTID)

object BlockDirectory {
  val blocks: List[Block] = List[Block](Air, Stone, Dirt, Brick, Grass)

  val lookup: Map[Byte, Block] = blocks map (b => (b.id, b)) toMap

  def apply(b: Byte): Block = lookup(b)
}
