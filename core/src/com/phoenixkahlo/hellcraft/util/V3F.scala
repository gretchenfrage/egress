package com.phoenixkahlo.hellcraft.util

import com.badlogic.gdx.math.Vector3

/**
  * A vector of 3 floats
  */
class V3F(val x: Float, val y: Float, val z: Float) {

  def +(o: V3F): V3F =
    V3F(x + o.x, y + o.y, z + o.z)

  def -(o: V3F): V3F =
    V3F(x - o.x, y - o.y, z - o.z)

  def neg: V3F =
    V3F(-x, -y, -z)

  def >(o: V3F): Boolean =
    x > o.x && y > o.y && z > o.z

  def <(o: V3F): Boolean =
    x < o.x && y < o.y && z < o.z

  def >=(o: V3F): Boolean =
    x >= o.x && y >= o.y && z >= o.z

  def <=(o: V3F): Boolean =
    x <= o.x && y <= o.y && z <= o.z

  def *(s: Float): V3F =
    V3F(x * s, y * s, z * s)

  lazy val toGdx = new Vector3(x, y, z)

  protected def toIntsStrategy =
    V3I(x.toInt, y.toInt, z.toInt)
  lazy val toInts = toIntsStrategy

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case V3F(xx, yy, zz) => x == xx && y == yy && z == zz
      case _ => false
    }

  override def hashCode(): Int = (x, y, z).hashCode()

  override def toString: String =
    "<" + x + ", " + y + ", " + z + ">"

}

object V3F {

  def apply(x: Float, y: Float, z: Float) =
    new V3F(x, y, z)

  def unapply(v: V3F): Option[(Float, Float, Float)] =
    Some((v.x, v.y, v.z))

}