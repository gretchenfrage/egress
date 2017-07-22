package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.gamedriver.GameDriver

import scala.concurrent.duration._

class GametimeClock(getNanotime: => Long, val nanotimeStart: Long) {

  def gametime: Long = (((getNanotime - nanotimeStart) nanoseconds) / GameDriver.dt) toLong

}

object GametimeClock {

  def serverClock(): GametimeClock =
    new GametimeClock(System.nanoTime, System.nanoTime)

  def clientClock(session: ServerSession, nanotimeMirror: NanotimeMirror): GametimeClock =
    new GametimeClock(nanotimeMirror.nanotime, session.getGameclockStartNanotime)

}