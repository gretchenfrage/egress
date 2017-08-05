package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.gamedriver.GameDriver

import scala.concurrent.duration._

class GametimeClock(getNanotime: => Long, val nanotimeStart: Long) {

  def gametime: Long = (((getNanotime - nanotimeStart) nanoseconds) / GameDriver.dt) toLong

  def waitUntil(time: Long): Unit = {
    val duration = (time * GameDriver.dt) + (nanotimeStart nanoseconds) - (getNanotime nanoseconds)
    if (duration > Duration.Zero)
      Thread.sleep(duration toMillis)
  }

  def timeSince(time: Long): Duration =
    ((System.nanoTime() - nanotimeStart) nanoseconds) - (time * GameDriver.dt)

  def fractionalTicksSince(time: Long): Float =
    (timeSince(time) / GameDriver.dt) toFloat

}

object GametimeClock {

  def serverClock(): GametimeClock =
    new GametimeClock(System.nanoTime, System.nanoTime)

  def clientClock(session: ServerSession, nanotimeMirror: NanotimeMirror): GametimeClock =
    new GametimeClock(nanotimeMirror.nanotime, session.getGameclockStartNanotime)

}