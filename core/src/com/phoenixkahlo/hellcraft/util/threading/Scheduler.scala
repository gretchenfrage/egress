package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

import scala.concurrent.duration.Duration


object Scheduler {

  val executor = new ScheduledThreadPoolExecutor(1)

  def schedule(task: Runnable, delay: Duration): Unit =
    executor.schedule(task, delay.toNanos, TimeUnit.NANOSECONDS)

}