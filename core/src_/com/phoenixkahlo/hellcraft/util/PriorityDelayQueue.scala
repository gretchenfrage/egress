package com.phoenixkahlo.hellcraft.util

import java.util
import java.util.concurrent._
import scala.concurrent.duration._

import scala.concurrent.duration.Duration

class PriorityDelayQueue[E <: Delayed] extends PriorityBlockingQueue[E] {

  private val executor = new ScheduledThreadPoolExecutor(1)

  override def offer(e: E): Boolean = {
    executor.schedule(() => {
      PriorityDelayQueue.super.offer(e)
    }, e.getDelay(TimeUnit.NANOSECONDS), TimeUnit.NANOSECONDS)
    true
  }

}