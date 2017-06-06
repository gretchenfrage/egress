package com.phoenixkahlo.hellcraft.util

case class V3I(x: Int, y: Int, z: Int) {

  def +(o: V3I): V3I =
    V3I(x + o.x, y + o.y, z + o.z)

  def -(o: V3I): V3I =
    V3I(x - o.x, y - o.y, z - o.z)

  def neg: V3I =
    V3I(-x, -y, -z)

  def toFloats: V3F =
    V3F(x, y, z)

}

