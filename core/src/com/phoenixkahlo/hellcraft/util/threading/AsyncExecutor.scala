package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{ExecutionContext, Future}

private class ScalingQueue extends LinkedBlockingQueue[Runnable] {

  var executor: ThreadPoolExecutor = _

  override def offer(e: Runnable): Boolean = {
    val allWorkingThreads = executor.getActiveCount + super.size
    allWorkingThreads < executor.getPoolSize && super.offer(e)
  }

}

private class ForceQueuePolicy extends RejectedExecutionHandler {
  override def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor): Unit = {
    try {
      executor.getQueue.put(r)
    } catch {
      case e: InterruptedException => throw new RejectedExecutionException(e)
    }
  }
}

private class ScalingThreadPoolExecutor(factory: ThreadFactory)
  extends ThreadPoolExecutor(1, Integer.MAX_VALUE, 1, TimeUnit.SECONDS, new ScalingQueue, factory) {

  super.setRejectedExecutionHandler(new ForceQueuePolicy)
  super.getQueue.asInstanceOf[ScalingQueue].executor = this

  private val activeCount = new AtomicInteger

  override def getActiveCount: Int = activeCount.get

  override def beforeExecute(t: Thread, r: Runnable): Unit = activeCount.incrementAndGet()

  override def afterExecute(r: Runnable, t: Throwable): Unit = activeCount.decrementAndGet()

}

object AsyncExecutor {

  def apply(threadName: String = "async thread"): ExecutorService = {
    val counter = new AtomicInteger(0)
    new ScalingThreadPoolExecutor(runnable => new Thread(runnable, threadName + " #" + counter.incrementAndGet()))
  }

  def context(threadName: String = "async thread"): ExecutionContext =
    ExecutionContext.fromExecutor(AsyncExecutor(threadName))

  val global: ExecutionContext = context("global async thread")

  def run[T](task: => T): Future[T] =
    Future { task } (global)

}

object AsyncExecutorTest {

  def main(args: Array[String]): Unit = {
    val executor = AsyncExecutor("hellfire")
    for (i <- 1 to 100) {
      executor.execute(() => {
        Thread.sleep(1000)
        println(i)
        Thread.sleep(1000)
      })
    }
  }

}
