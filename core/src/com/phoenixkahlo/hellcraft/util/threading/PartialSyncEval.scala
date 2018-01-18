package com.phoenixkahlo.hellcraft.util.threading

import java.lang.reflect.Method
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}

import com.badlogic.gdx.Gdx


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

object PartialSyncGLEval {
  val method: Method = Gdx.app.getClass.getMethod("executeRunnables")

  def apply[T](fut: Fut[T]): T = {
    while (fut.query.isEmpty)
      method.invoke(Gdx.app)
    fut.query.get
  }
}