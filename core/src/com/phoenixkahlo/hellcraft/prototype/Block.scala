package com.phoenixkahlo.hellcraft.prototype

import com.phoenixkahlo.hellcraft.util.V2F

import scala.collection.immutable.Map

/**
  * Created by kahlo on 6/10/2017.
  */
sealed abstract class Block(
                      val id: Byte,
                      val texID: Int,
                      val isOpaque: Boolean = true,
                      val isCorporeal: Boolean = true
                    ) {

  def isTranslucent = !isOpaque

  def texCoord1: V2F =
    V2F(texID % 16, (texID - texID % 16) / 16) / 16

  def texCoord2: V2F =
    texCoord1 + V2F(1f / 16f, 0)

  def texCoord3: V2F =
    texCoord1 + V2F(1f / 16f, 1f / 16f)

  def texCoord4: V2F =
    texCoord1 + V2F(0, 1f / 16f)

}

case object Dirt extends Block(2, 20)

case object Stone extends Block(1, 17)

case object Air extends Block(0, -1, isOpaque = false, isCorporeal = false)

object BlockDirectory {
  val blocks: List[Block] = List[Block](
    Air, Stone, Dirt
  )

  val lookup: Map[Byte, Block] = blocks map (b => (b.id, b)) toMap

  def apply(b: Byte): Block = lookup(b)
}
