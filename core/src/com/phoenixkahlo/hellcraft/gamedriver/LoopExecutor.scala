package com.phoenixkahlo.hellcraft.gamedriver

import scala.concurrent.duration.DurationInt

trait LoopingApp {

  val fps: Int = UpdatingGameDriver.updatesPerSecond

  def init(deactivator: Runnable): Unit

  def update(): Unit

  def dispose(): Unit

}

class LoopExecutor(app: LoopingApp) extends Thread("looping app") {

  private val dt: Long = ((1 second) / app.fps) toNanos

  private var lastTime: Long = _
  private var timeDebt: Long = 0

  @volatile private var shouldContinue = true

  override def run(): Unit = {
    app.init(() => shouldContinue = false)
    lastTime = System.nanoTime()
    try {
      while (shouldContinue) {
        val n = timeDebt / dt
        timeDebt -= n * dt

        for (_ <- 1 to n.toInt)
          app.update()

        val currentTime = System.nanoTime()
        timeDebt += currentTime - lastTime
        lastTime = currentTime

        val sleepTime = ((dt - timeDebt).toInt nanoseconds).toMillis
        if (sleepTime > 0)
          Thread.sleep(sleepTime)
      }
    } finally app.dispose()
  }

}