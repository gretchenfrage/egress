package com.phoenixkahlo.hellcraft.math

import com.badlogic.gdx.graphics.Color
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode

/**
  * A vector of 3 ints, that is also a vector of 3 floats
  */
@CarboniteWith(classOf[FieldNode])
class V3I(val xi: Int, val yi: Int, val zi: Int) extends V3F(xi, yi, zi) {

  def +(o: V3I): V3I =
    V3I(xi + o.xi, yi + o.yi, zi + o.zi)

  def -(o: V3I): V3I =
    V3I(xi - o.xi, yi - o.yi, zi - o.zi)

  def **(o: V3I): V3I =
    V3I(xi * o.xi, yi * o.yi, zi * o.zi)

  override def neg: V3I =
    V3I(-xi, -yi, -zi)

  def >(o: V3I): Boolean =
    xi > o.xi && yi > o.yi && zi > o.zi

  def <(o: V3I): Boolean =
    xi < o.xi && yi < o.yi && zi < o.zi

  def >=(o: V3I): Boolean =
    xi >= o.xi && yi >= o.yi && zi >= o.zi

  def <=(o: V3I): Boolean =
    xi <= o.xi && yi <= o.yi && zi <= o.zi

  def dim: Option[Int] =
    if (xi == yi && yi == zi) Some(xi)
    else None

  def surrounding: Seq[V3I] =
    ((this - Ones) to (this + Ones)) filterNot (this ==)

  override protected def toIntsStrategy: V3I =
    this

  def *(s: Int): V3I =
    V3I(xi * s, yi * s, zi * s)

  def until(o: V3I) =
    for {
      x <- xi until o.xi
      y <- yi until o.yi
      z <- zi until o.zi
    } yield V3I(x, y, z)

  def to(o: V3I) =
    for {
      x <- xi to o.xi
      y <- yi to o.yi
      z <- zi to o.zi
    } yield V3I(x, y, z)

  def fold(f: (Int, Int) => Int) =
    f(xi, f(yi, zi))

  def %(s: Int): V3I =
    if (this >= Origin) V3I(xi % s, yi % s, zi % s)
    else (V3I(xi % s, yi % s, zi % s) + V3I(s, s, s)) % s

  override def toString: String =
    "<" + xi + ", " + yi + ", " + zi + ">"

  def decompress(i: Int): V3I = {
    val y = i / (xi * zi)
    val z = (i % (xi * zi)) / xi
    val x = i % xi
    V3I(x, y, z)
  }

  override lazy val flatten: V2I =
    V2I(xi, zi)

  def compress(v: V3I): Int = {
    v.xi + v.zi * xi + v.yi * xi * zi
  }

  @transient lazy val toColor: Color = this / 256 match { case V3F(r, g, b) => new Color(r, g, b, 1) }

  def touching: Seq[V3I] =
    Directions().map(this + _)

  def neighbors: Seq[V3I] =
    (Ones.neg to Ones).map(_ + this)

}

object Origin extends V3I(0, 0, 0)

object Ones extends V3I(1, 1, 1)

object V3I {

  def apply(x: Int, y: Int, z: Int) =
    new V3I(x, y, z)

  def apply(color: Color) =
    new V3I(color.r * 256 toInt, color.g * 256 toInt, color.b * 256 toInt)

  def unapply(v: V3I): Option[(Int, Int, Int)] =
    Some((v.xi, v.yi, v.zi))

}