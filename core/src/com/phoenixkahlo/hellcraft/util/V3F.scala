package com.phoenixkahlo.hellcraft.util

import com.badlogic.gdx.math.Vector3

case class V3F(x: Float, y: Float, z: Float) {

  def +(o: V3F): V3F =
    V3F(x + o.x, y + o.y, z + o.z)

  def -(o: V3F): V3F =
    V3F(x - o.x, y - o.y, z - o.z)

  def neg: V3F =
    V3F(-x, -y, -z)

  def toGdx: Vector3 =
    new Vector3(x, y, z)

}
