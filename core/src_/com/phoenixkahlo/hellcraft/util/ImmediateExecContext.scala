package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

/**
  * Executes tasks with as little delay as possible by feeding them into a cached dynamic thread pool with maximum
  * thread priority.
  */
class ImmediateExecContext {

  val context = ExecutionContext.fromExecutor(Executors.newCachedThreadPool(
    runnable => {
      val thread = new Thread(runnable)
      thread.setPriority(Thread.MAX_PRIORITY)
      thread
    }
  ))

  def apply(): ExecutionContext = context

}
