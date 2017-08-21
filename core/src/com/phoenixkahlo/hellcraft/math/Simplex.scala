package com.phoenixkahlo.hellcraft.math

import java.util.concurrent.ThreadLocalRandom

import other.OpenSimplexNoise

case class Simplex(stretch: Float, amp: Float, seed: Long = ThreadLocalRandom.current.nextLong()) {

  val service = new OpenSimplexNoise(seed)

  def apply(v: V2F): Float = {
    (service.eval(v.x * stretch, v.y * stretch).toFloat + 1) / 2 * amp
  }

  def apply(v: V3F): Float = {
    (service.eval(v.x * stretch, v.y * stretch, v.z * stretch).toFloat + 1) / 2 * amp
  }

  def apply(v: V4F): Float = {
    (service.eval(v.x * stretch, v.y * stretch, v.z * stretch, v.w * stretch).toFloat + 1) / 2 * amp
  }

}