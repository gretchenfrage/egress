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

  lazy val toGdx = new Vector3(x, y, z)

  protected def toIntsStrategy =
    V3I(x.toInt, y.toInt, z.toInt)
  lazy val toInts = toIntsStrategy

}

object V3F {

  def apply(x: Float, y: Float, z: Float) =
    new V3F(x, y, z)

}