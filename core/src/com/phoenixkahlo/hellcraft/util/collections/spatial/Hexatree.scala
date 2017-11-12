package com.phoenixkahlo.hellcraft.util.collections.spatial

import com.phoenixkahlo.hellcraft.math.{V3F, V4F, V4I}
/*
sealed trait Hexatree[+E] extends Map[V4F, E] {
  override def +[V1 >: E](kv: (V4F, V1)): Hexatree[V1]

  override def -(key: V4F): Hexatree[E]

  def closest(point: V4F, within: Float): Option[(V3F, E)]

  def within(point: V3F, within: Float): Seq[(V3F, E)]

  override def size: Int

  def hexant: Hexant
}

private object HexaSigns {
  def indexOf(sign: V4I): Int = {
    ((sign.xi & 0x2) >> 1) | (sign.yi & 0x2) | ((sign.zi & 0x2) << 1) | ((sign.wi & 0x2) << 2)
  }

  val signs = new Array[V4I](16)
  for {
    x <- Seq(-1, 1)
    y <- Seq(-1, 1)
    z <- Seq(-1, 1)
    w <- Seq(-1, 1)
  } yield {
    val sign = V4I(x, y, z, w)
    signs(indexOf(sign)) = sign
  }

  def signOf(diff: V4F): V4I =
    diff.map(n => if (n > 0) 1 else -1).toInts
}

case class Hexant(center: V4F, range: Float) {
  val min: V4F = center - V4F(range, range, range, range)
  val max: V4F = center + V4F(range, range, range, range)
  val diagonal: Float = Math.sqrt(range * range * 16).toFloat

  def contains(v: V4F): Boolean =
    v >= min && v <= max

  def subsign(v: V4F): V4I =
    HexaSigns.signOf(v - center)

  def subhexant(subSign: V4I): Hexant =
    Hexant(center + (subSign * range / 2), range / 2)

  def maxdist(v: V4F): Float =
    HexaSigns.signs.toSeq.map(sign => )
}*/