package com.phoenixkahlo.hellcraft.math

import java.util.UUID

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

  def nextLong: (RNG, Long) =
    nextInt match { case (rng1, n1) => rng1.nextInt match { case (rng2, n2) => (rng2, n1.toLong << 32 | n2.toLong) } }

  def nextUUID: (RNG, UUID) =
    nextLong match { case (rng1, n1) => rng1.nextLong match { case (rng2, n2) => (rng2, new UUID(n1, n2)) } }

}

object RNG {

  def ints(rng: RNG): Stream[Int] =
    rng.nextInt match { case (nrng, n) => Stream.cons(n, ints(nrng)) }

  def floats(rng: RNG): Stream[Float] =
    rng.nextFloat match { case (nrng, n) => Stream.cons(n, floats(nrng)) }

  def longs(rng: RNG): Stream[Long] =
    rng.nextLong match { case (nrng, n) => Stream.cons(n, longs(nrng)) }

  def uuids(rng: RNG): Stream[UUID] =
    rng.nextUUID match { case (nrng, uuid) => Stream.cons(uuid, uuids(nrng)) }

  def meta[T](metaRNG: RNG, f: RNG => Stream[T]): Stream[Stream[T]] = {
    metaRNG.nextLong match {
      case (nextMetaRNG, nextSeed) =>
        Stream.cons(f(RNG(nextSeed)), meta(nextMetaRNG, f))
    }
  }

}