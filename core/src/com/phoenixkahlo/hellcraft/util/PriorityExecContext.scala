package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.{ExecutorService, Executors}

import scala.collection.immutable
import scala.concurrent.ExecutionContext

/**
  * Fixed size thread pool executors for each of the 10 priority levels, with a thread count equal to the runtime's
  * logical core count.
  */
object PriorityExecContext {

  val services: Seq[ExecutorService] = (1 to 10).map(priority => Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors(),
    runnable => {
      val thread = new Thread(runnable)
      thread.setPriority(priority)
      thread
    }
  ))
  val wrapped: Seq[ExecutionContext] = services.map(ExecutionContext.fromExecutor)

  def apply(priority: Int): ExecutionContext = wrapped(priority - 1)

}
