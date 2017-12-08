package com.phoenixkahlo.hellcraft.math

import com.badlogic.gdx.graphics.Color

class V4F(val x: Float, val y: Float, val z: Float, val w: Float) extends Serializable {

  def +(o: V4F): V4F =
    V4F(x + o.x, y + o.y, z + o.z, w + o.w)

  def -(o: V4F): V4F =
    V4F(x - o.x, y - o.y, z - o.z, w - o.w)

  def neg: V4F =
    V4F(-x, -y, -z, -w)

  def *(s: Float): V4F =
    V4F(x * s, y * s, z * s, w * s)

  def /(s: Float): V4F =
    V4F(x / s, y / s, z / s, w / s)

  def flatten: V3F =
    V3F(x, y, z)
  
  def >(o: V4F): Boolean =
    x > o.x && y > o.y && z > o.z && w > o.w

  def <(o: V4F): Boolean =
    x < o.x && y < o.y && z < o.z && w < o.w

  def >=(o: V4F): Boolean =
    x >= o.x && y >= o.y && z >= o.z && w >= o.w

  def <=(o: V4F): Boolean =
    x <= o.x && y <= o.y && z <= o.z && w <= o.w

  def magnitude: Float =
    Math.sqrt(x * x + y * y + z * z + w * w).toFloat

  def dist(v: V4F): Float =
    (this - v).magnitude

  def normalize: V4F =
    this / magnitude

  def map(func: Float => Float) =
    V4F(func(x), func(y), func(z), func(w))

  def fold(f: (Float, Float) => Float): Float =
    f(f(x, y), f(z, w))

  protected def toIntsStrategy: V4I =
    V4I(x.toInt, y.toInt, z.toInt, w.toInt)
  lazy val toInts = toIntsStrategy

  override def hashCode(): Int =
    (x, y, z, w).hashCode()

  def toColor: Color = new Color(x, y, z, w)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case V4F(xx, yy, zz, ww) => x == xx && y == yy && z == zz && w == ww
      case _ => false
    }

  override def toString: String =
    "<" + x + ", " + y + ", " + z + ", " + w + ">"
}

object V4F {
  def apply(x: Float, y: Float, z: Float, w: Float): V4F =
    new V4F(x, y, z, w)

  def unapply(v: V4F): Option[(Float, Float, Float, Float)] =
    Some((v.x, v.y, v.z, v.w))

  def unpackColor(f: Float): V4F = {
    val i = java.lang.Float.floatToIntBits(f)
    val r = (i & 0xFF) / 255f
    val g = ((i & 0xFF00) >> 8) / 255f
    val b = ((i & 0xFF0000) >> 16) / 255f
    val a = ((i % 0xFF000000) >> 24) / 255f
    V4F(r, g, b, a)
  }
}