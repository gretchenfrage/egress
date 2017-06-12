package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.V2F

import scala.collection.immutable.Map

/**
  * The block singletons.
  */
sealed abstract class Block(
                      val id: Byte,
                      val tid: TextureID,
                      val isOpaque: Boolean = true,
                      val isCorporeal: Boolean = true
                    ) {

  def isTranslucent = !isOpaque

}

case object Air extends Block(0, null, isOpaque = false, isCorporeal = false)

case object Stone extends Block(1, StoneTID)

case object Dirt extends Block(2, DirtTID)

object BlockDirectory {
  val blocks: List[Block] = List[Block](Air, Stone, Dirt)

  val lookup: Map[Byte, Block] = blocks map (b => (b.id, b)) toMap

  def apply(b: Byte): Block = lookup(b)
}
