package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

/**
  * Fixed size thread pool executors for each of the 10 priority levels, with a thread count equal to the runtime's
  * logical core count.
  */
object PriorityExecContext {

  val contexts = (1 to 10).map(priority => ExecutionContext.fromExecutor(Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors(),
    runnable => {
      val thread = new Thread(runnable)
      thread.setPriority(priority)
      thread
    }
  )))

  def apply(priority: Int): ExecutionContext = contexts(priority - 1)

}
