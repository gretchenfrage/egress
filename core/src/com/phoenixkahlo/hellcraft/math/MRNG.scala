package com.phoenixkahlo.hellcraft.math

import java.util.UUID

class MRNG(val seed: MRNG.Seed) {
  private var rng = RNG(seed)

  def nextInt: Int = {
    val (next, n) = rng.nextInt
    rng = next
    n
  }

  def nextFloat: Float = {
    val (next, n) = rng.nextFloat
    rng = next
    n
  }

  def nextLong: Long = {
    val (next, n) = rng.nextLong
    rng = next
    n
  }

  def nextSeed: MRNG.Seed = nextLong

  def nextUUID: UUID = {
    val (next, id) = rng.nextUUID
    rng = next
    id
  }

}

object MRNG {
  type Seed = Long
}