package com.phoenixkahlo.hellcraft.util

/**
  * Purely functional random number generator
  */
case class RNG(seed: Long) {

  def nextInt: (RNG, Int) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    (RNG(nextSeed), (nextSeed >>> 16).asInstanceOf[Int])
  }

  def nextFloat: (RNG, Float) =
    nextInt match { case (rng, n) => (rng, java.lang.Float.intBitsToFloat(n)) }

}

object RNG {

  def ints(rng: RNG): Stream[Int] =
    rng.nextInt match { case (nrng, n) => Stream.cons(n, ints(nrng)) }

  def floats(rng: RNG): Stream[Float] =
    rng.nextFloat match { case (nrng, n) => Stream.cons(n, floats(nrng)) }

}