package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.util.debugging.Profiler

import scala.util.Random

object SqrtProfileTest extends App {

  val rand = new Random()
  for (_ <- 1 to 100) {
    val n = rand.nextFloat()
    val p = Profiler("sqrt")
    val r = Math.sqrt(n).toFloat
    p.log()
    p.printMicro()
  }

}
