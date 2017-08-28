package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode

@CarboniteWith(classOf[FieldNode])
case class V4F(x: Float, y: Float, z: Float, w: Float) {

  def +(o: V4F): V4F =
    V4F(x + o.x, y + o.y, z + o.z, w + o.w)

  def -(o: V4F): V4F =
    V4F(x - o.x, y - o.y, z + o.z, w + o.w)

  def neg: V4F =
    V4F(-x, -y, -z, -w)

  def *(s: Float): V4F =
    V4F(x * s, y * s, z * s, w * s)

  def /(s: Float): V4F =
    V4F(x / s, y / s, z / s, w / s)

  def flatten: V3F =
    V3F(x, y, z)

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

}
