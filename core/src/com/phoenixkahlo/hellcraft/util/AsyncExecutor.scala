package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.{ExecutorService, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

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

}
