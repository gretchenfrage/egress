package com.phoenixkahlo.hellcraft.gamedriver

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.ApplicationAdapter

import scala.concurrent.duration._

class GameDriver(state: GameState) extends ApplicationAdapter {

  private var lastRenderTime: Long = -1 // nano time
  private var tickTimeDebt: Duration = Duration.Zero
  private val updatedFlag = new AtomicBoolean(true)
  @volatile private var running = true

  override def create(): Unit = {
    state.onEnter()

    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    val thread = new Thread(() => {
      lastRenderTime = System.nanoTime()
      while (running) {
        val currRenderTime = System.nanoTime()
        tickTimeDebt += ((currRenderTime - lastRenderTime) nanoseconds)
        val sleepFor = GameDriver.dt - tickTimeDebt
        if (sleepFor > Duration.Zero)
          Thread.sleep(sleepFor toMillis)
        while (tickTimeDebt >= GameDriver.dt) {
          if (state.update()) {
            updatedFlag.set(true)
            updatedFlag.synchronized {
              updatedFlag.notifyAll()
            }
          }
          tickTimeDebt -= GameDriver.dt
        }
        lastRenderTime = currRenderTime
      }
    }, "game updating thread")
    thread.setPriority(Thread.MAX_PRIORITY)
    thread.start()

  }

  override def render(): Unit = {
    updatedFlag.synchronized {
      while (!updatedFlag.getAndSet(false))
        updatedFlag.wait()
    }
    state.render()
  }

  override def dispose(): Unit = {
    running = false
    state.onExit()
  }

}

object GameDriver {

  val updatesPerSecond: Int = 60
  val dt: Duration = (1 second) / updatesPerSecond

}