package com.phoenixkahlo.hellcraft.isotest

import com.phoenixkahlo.hellcraft.math.V3I
import other.OpenSimplexNoise

object SimplexNoiseTest extends App {

  val noise = new OpenSimplexNoise

  val range = V3I(-50, -50, -50) to V3I(50, 50, 50)
  val samples = range.map(v => noise.eval(v.x, v.y, v.z))
  println("min=" + samples.min)
  println("max=" + samples.max)

}
