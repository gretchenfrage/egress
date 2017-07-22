package com.phoenixkahlo.hellcraft.util

import java.util.PriorityQueue
import java.util.Comparator
import java.util.concurrent.{DelayQueue, Delayed, ScheduledThreadPoolExecutor, TimeUnit}
import java.util.function.ToLongFunction

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.concurrent.duration._


object Scheduler {

  val executor = new ScheduledThreadPoolExecutor(1)

  def schedule(task: Runnable, delay: Duration): Unit =
    executor.schedule(task, delay.toNanos, TimeUnit.NANOSECONDS)

}