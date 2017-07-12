package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.concurrent.{ExecutorService, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

object NetworkExecutor {

  def apply(threadName: String = "network thread"): ExecutorService = new ThreadPoolExecutor(
    1,
    Int.MaxValue,
    1, TimeUnit.MINUTES,
    new LinkedBlockingQueue,
    runnable => {
      new Thread(runnable, threadName)
    }
  )

}
