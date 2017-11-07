package com.phoenixkahlo.hellcraft.math.physics

import com.phoenixkahlo.hellcraft.math.Trig

object Quadratic {

  def getLowestRoot(a: Float, b: Float, c: Float): Option[Float] = {
    val determinant = b * b - 4 * a * c
    if (determinant < 0) None
    else {
      val sqrtD = Math.sqrt(determinant).toFloat
      var (r1, r2) = Trig.sort(((-b - sqrtD) / (2 * a), (-b + sqrtD) / (2 * a)))
      if (r1 > 0) Some(r1)
      else if (r2 > 0) Some(r2)
      else None
    }
  }

}
