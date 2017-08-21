package com.phoenixkahlo.hellcraft.singleplayer

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._

class GametimeClock() {

  private var nanotimeStart: Long = System.nanoTime()

  def reset(): Unit = {
    nanotimeStart = System.nanoTime()
  }

  def forgive(amount: Duration): Unit = {
    nanotimeStart += amount.toNanos
  }

  def gametime: Long = (((System.nanoTime() - nanotimeStart) nanoseconds) / Delta.dt) toLong

  def waitUntil(time: Long): Unit = {
    val duration = (time * Delta.dt) + (nanotimeStart nanoseconds) - (System.nanoTime() nanoseconds)
    if (duration > Duration.Zero)
      Thread.sleep(duration toMillis)
  }

  def timeSince(time: Long): Duration =
    ((System.nanoTime() - nanotimeStart) nanoseconds) - (time * Delta.dt)

  def fractionalTicksSince(time: Long): Float =
    (timeSince(time) / Delta.dt) toFloat

}