package com.phoenixkahlo.hellcraft.server

import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt

/**
  * keep in mind: the timer starts when the object is created
  */
class LoopExecutor(dtDuration: Duration, task: () => Unit) extends Thread {

  val dt: Long = dtDuration toNanos

  var lastTime: Long = System.nanoTime()
  var timeDebt: Long = 0

  override def run(): Nothing = {
    println("thread starting")
    while (true) {
      val n = timeDebt / dt
      timeDebt -= n * dt

      for (_ <- 1 to n.toInt) task()

      val currentTime = System.nanoTime()
      timeDebt += currentTime - lastTime
      lastTime = currentTime

      val sleepTime = ((dt - timeDebt).toInt nanoseconds).toMillis
      if (sleepTime > 0)
        Thread.sleep(sleepTime)
    }
    ???
  }

}