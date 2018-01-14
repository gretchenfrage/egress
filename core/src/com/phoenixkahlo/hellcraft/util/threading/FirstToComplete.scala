package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.mutable.ArrayBuffer

class FirstToComplete[T](set: Set[Fut[T]]) extends Fut[(T, Set[Fut[T]])] {
  @volatile private var status: Option[(T, Set[Fut[T]])] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object
  private val triggered = new AtomicBoolean(false)

  for (fut <- set) fut.onComplete(() => {
    if (!triggered.getAndSet(true)) {
      val done = fut.query.get
      val rest = set - fut
      monitor.synchronized {
        status = Some((done, rest))
        monitor.notifyAll()
      }
      listeners.foreach(_.run())
    }
  })

  override def await: (T, Set[Fut[T]]) = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[(T, Set[Fut[T]])] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }
}

object FirstToComplete {
  def apply[T](set: Set[Fut[T]]): Fut[(T, Set[Fut[T]])] = new FirstToComplete[T](set)
}