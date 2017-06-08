package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.V2F

import scala.collection.immutable.Map

sealed abstract class Block(
                      val id: Byte,
                      val isOpaque: Boolean = true
                    ) {
  def isTranslucent = !isOpaque
}

case object Air extends Block(0, isOpaque = false)
case object Stone extends Block(1)

object BlockDirectory {
  val blocks: List[Block] = List[Block](
    Air, Stone
  )

  val lookup: Map[Byte, Block] = blocks map (b => (b.id, b)) toMap
}