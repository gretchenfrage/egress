package com.phoenixkahlo.hellcraft.gamedriver

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.ApplicationAdapter

import scala.concurrent.duration._

class GameDriver(state: GameState) extends ApplicationAdapter {

  private var lastRenderTime: Long = -1 // nano time
  private var tickTimeDebt: Duration = Duration.Zero
  private val updatedFlag = new AtomicBoolean(true)

  override def create(): Unit = {
    state.onEnter()

    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    val thread = new Thread(() => {
      lastRenderTime = System.nanoTime()
      while (true) {
        val currRenderTime = System.nanoTime()
        tickTimeDebt += ((currRenderTime - lastRenderTime) nanoseconds)
        /*
        val sleepFor = GameDriver.dt - tickTimeDebt
        if (sleepFor > Duration.Zero)
          Thread.sleep(sleepFor toMillis)
        */
        while (tickTimeDebt >= GameDriver.dt) {
          println("updating, debt = " + tickTimeDebt.toUnit(TimeUnit.SECONDS))
          state.update()
          updatedFlag.set(true)
          updatedFlag.synchronized { updatedFlag.notifyAll() }
          tickTimeDebt -= GameDriver.dt
        }
        lastRenderTime = currRenderTime
      }
    })
    thread.setPriority(Thread.NORM_PRIORITY)
    thread.start()

  }

  override def render(): Unit = {
    println("rendering")
    updatedFlag.synchronized {
      while (!updatedFlag.getAndSet(false))
        updatedFlag.wait()
    }
    state.render()
    /*
    if (lastRenderTime == -1) {
      lastRenderTime = System.nanoTime()
    } else {
      // update
      val currRenderTime = System.nanoTime()
      tickTimeDebt += ((currRenderTime - lastRenderTime) nanoseconds)
      while (tickTimeDebt >= GameDriver.dt) {
        state.update()
        tickTimeDebt -= GameDriver.dt
      }
      // render
      state.render()
      // prepare
      lastRenderTime = currRenderTime
    }
    */
    /*
    val s = System.nanoTime()
    state.update()
    state.render()
    val e = System.nanoTime()
    println("dt = " + ((e - s) / 1000000))
    */

  }

  override def dispose(): Unit = {
    state.onExit()
  }

}

object GameDriver {

  val updatesPerSecond: Int = 60
  val dt: Duration = (1 second) / updatesPerSecond

}