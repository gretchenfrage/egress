package com.phoenixkahlo.hellcraft.util.time

import scala.concurrent.duration._

case class Timer(start: Long) {

  def elapsed: Duration = (System.nanoTime - start) nanoseconds

}

object Timer {
  def start: Timer = Timer(System.nanoTime)
}