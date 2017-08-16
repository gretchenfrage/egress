package com.phoenixkahlo.hellcraft.gamedriver

import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.ApplicationAdapter

import scala.concurrent.duration._

object Delta {

  val updatesPerSecond: Int = 20
  var dt: Duration = (1 second) / updatesPerSecond

}