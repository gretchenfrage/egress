package com.phoenixkahlo.hellcraft.singleplayer

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import com.phoenixkahlo.hellcraft.gamedriver.Delta

import scala.concurrent.duration._

trait GametimeClock {
  def reset(): Unit
  def forgive(amount: Duration)
  def gametime: Long
  def fgametime: Float
  def waitUntil(time: Long): Unit
  def timeSince(time: Long): Duration
  def fractionalTicksSince(time: Long): Float
}

class DefGametimeClock extends GametimeClock {
  private var nanotimeStart: Long = System.nanoTime()

  override def reset(): Unit = {
    nanotimeStart = System.nanoTime()
  }

  override def forgive(amount: Duration): Unit = {
    nanotimeStart += amount.toNanos
  }

  override def gametime: Long = (((System.nanoTime() - nanotimeStart) nanoseconds) / Delta.dt) toLong

  override def fgametime: Float = (((System.nanoTime() - nanotimeStart) nanoseconds) / Delta.dt) toFloat

  override def waitUntil(time: Long): Unit = {
    val duration = (time * Delta.dt) + (nanotimeStart nanoseconds) - (System.nanoTime() nanoseconds)
    if (duration > Duration.Zero)
      Thread.sleep(duration toMillis)
  }

  override def timeSince(time: Long): Duration =
    ((System.nanoTime() - nanotimeStart) nanoseconds) - (time * Delta.dt)

  override def fractionalTicksSince(time: Long): Float =
    (timeSince(time) / Delta.dt) toFloat
}

class ManualGametimeClock extends GametimeClock {
  private var n: Long = 0
  private var push = new LinkedBlockingQueue[Unit]

  def tick(): Unit = push.add(())

  override def reset(): Unit = n = 0

  override def forgive(amount: Duration): Unit = ()

  override def gametime: Long = n

  override def fgametime: Float = n

  override def waitUntil(time: Long): Unit = {
    while (n < time) {
      push.take()
      n += 1
    }
  }

  override def timeSince(time: Long): Duration = Duration.Zero

  override def fractionalTicksSince(time: Long): Float = 0
}