package com.phoenixkahlo.hellcraft.util.threading

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The fut system is an alternative to scala and java's future systems, that gives more flexibility to their
  * means of execution. Instead of the means of execution being hard-coded into the future system, the creation or
  * transformation of a <code>Fut</code> acceps a <code>Runnable => Unit</code> function that will be used to execute it.
  * This allows futs to be executed in a Java <code>Executor</code>, a Scala <code>ExecutionContext</code>, some
  * alternative such as a spatially prioritized executor, in a new thread, or even by hitching off of other threads using
  * <code>_.run()</code> as the executor (that is optimal for cheap transformations).
  */
trait Fut[T] {
  def await: T

  def query: Option[T]

  def map[E](func: T => E, executor: Runnable => Unit): Fut[E] =
    new MapFut(this, func, executor)

  def map[E](func: T => E): Fut[E] =
    new MapFut(this, func, _.run())

  def flatMap[E](func: T => Fut[E]): Fut[E] =
    new FlatMapFut(this, func)

  def onComplete(runnable: Runnable): Unit

  def afterwards[N](factory: => N, executor: Runnable => Unit): Fut[N] =
    new SeqFut(this, factory, executor)

}

object Fut {

  def apply[T](factory: => T, executor: Runnable => Unit): Fut[T] = {
    new EvalFut(factory, executor)
  }

  def fromFuture[T](future: Future[T]): Fut[T] = new Fut[T] {
    override def await: T = Await.result(future, Duration.Inf)

    override def query: Option[T] =
      if (future.isCompleted) Some(Await.result(future, Duration.Zero))
      else None

    override def onComplete(runnable: Runnable): Unit =
      future.onComplete(_ => runnable.run())(ExecutionContext.global)
  }

}

private class EvalFut[T](factory: => T, executor: Runnable => Unit) extends Fut[T] {
  @volatile private var status: Option[T] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  executor(() => {
    val result = factory
    monitor.synchronized {
      status = Some(result)
      monitor.notifyAll()
    }
    listeners.foreach(_.run())
  })

  override def await: T = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[T] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }
}

case class Promise(task: Runnable, executor: Runnable => Unit) extends Fut[Unit] {
  @volatile private var done: Boolean = false
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  executor(() => {
    task.run()
    monitor.synchronized {
      done = true
      monitor.notifyAll()
    }
    listeners.foreach(_.run())
  })

  override def await: Unit = {
    if (!done) monitor.synchronized {
      while (!done) monitor.wait()
    }
  }

  override def query: Option[Unit] =
    if (done) Some(())
    else None

  override def onComplete(runnable: Runnable): Unit = {
    if (done) runnable.run()
    else monitor.synchronized {
      if (done) runnable.run()
      else listeners += runnable
    }
  }
}

private class SeqFut[T](last: Fut[_], factory: => T, executor: Runnable => Unit) extends Fut[T] {
  @volatile private var status: Option[T] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  last.onComplete(() => {
    executor(() => {
      val result = factory
      monitor.synchronized {
        status = Some(result)
        monitor.notifyAll()
      }
      listeners.foreach(_.run())
    })
  })

  override def await: T = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[T] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }

}

private class MapFut[S, R](source: Fut[S], func: S => R, executor: Runnable => Unit) extends Fut[R] {
  @volatile private var status: Option[R] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  source.onComplete(() => {
    executor(() => {
      val result = func(source.query.get)
      monitor.synchronized {
        status = Some(result)
        monitor.notifyAll()
      }
      listeners.foreach(_.run())
    })
  })

  override def await: R = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[R] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }
}

private class FlatMapFut[S, R](source: Fut[S], func: S => Fut[R]) extends Fut[R] {
  @volatile private var status: Option[R] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  source.onComplete(() => {
    val resultFuture = func(source.query.get)
    resultFuture.onComplete(() => {
      val result = resultFuture.query.get
      monitor.synchronized {
        status = Some(result)
        monitor.notifyAll()
      }
      listeners.foreach(_.run())
    })
  })

  override def await: R = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[R] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }
}
