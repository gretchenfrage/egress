package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.{ExecutorService, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.Try

object AsyncExecutor {

  def apply(threadName: String = "async thread"): ExecutorService = new ThreadPoolExecutor(
    1,
    Int.MaxValue,
    1, TimeUnit.MINUTES,
    new LinkedBlockingQueue,
    runnable => {
      new Thread(runnable, threadName)
    }
  )

  def context(threadName: String = "async thread"): ExecutionContext =
    ExecutionContext.fromExecutor(AsyncExecutor(threadName))

  val global: ExecutionContext = context("global async thread")

  def run(task: => Unit): Future[Unit] =
    Future { task } (global)

}
