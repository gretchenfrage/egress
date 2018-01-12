package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}


object PartialSyncEval {
  def apply[T](gen: (Runnable => Unit) => Fut[T]): T = {
    val queue = new LinkedBlockingQueue[Either[Runnable, Unit]]
    val fut = gen(task => queue.add(Left(task)))
    fut.onComplete(() => queue.add(Right(())))
    var continue = true
    while (continue) queue.take() match {
      case Left(task) => task.run()
      case Right(()) => continue = false
    }
    fut.query.get
  }
}
