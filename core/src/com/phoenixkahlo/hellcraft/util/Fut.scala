package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.ThreadFactory

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}
import com.thesamet.spatial.KDTreeMap

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait Fut[T] {
  def await: T

  def query: Option[T]

  def map[E](func: T => E, executor: Runnable => Unit): Fut[E] =
    new MapFut(this, func, executor)

  def map[E](func: T => E): Fut[E] =
    new MapFut(this, func, _.run())

  def flatMap[E](func: T => Fut[E], executor: Runnable => Unit): Fut[E] =
    new FlatMapFut(this, func, executor)

  def flatMap[E](func: T => Fut[E]): Fut[E] =
    new FlatMapFut(this, func, _.run())

  def onComplete(runnable: Runnable): Unit
}

object Fut {

  def apply[T](factory: => T, executor: Runnable => Unit): Fut[T] =
    new EvalFut(factory, executor)

  def fromFuture[T](future: Future[T]): Fut[T] = new Fut[T] {
    override def await: T = Await.result(future, Duration.Inf)

    override def query: Option[T] =
      if (future.isCompleted) Some(Await.result(future, Duration.Zero))
      else None

    override def onComplete(runnable: Runnable): Unit =
      future.onComplete(_ => runnable.run())(ExecutionContext.global)
  }

}

class EvalFut[T](factory: => T, executor: Runnable => Unit) extends Fut[T] {
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

class MapFut[S, R](source: Fut[S], func: S => R, executor: Runnable => Unit) extends Fut[R] {
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

class FlatMapFut[S, R](source: Fut[S], func: S => Fut[R], executor: Runnable => Unit) extends Fut[R] {
  @volatile private var status: Option[R] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  source.onComplete(() => {
    executor(() => {
      val resultFuture = func(source.query.get)
      resultFuture.onComplete(() => {
        executor(() => {
          val result = resultFuture.query.get
          monitor.synchronized {
            status = Some(result)
            monitor.notifyAll()
          }
          listeners.foreach(_.run())
        })
      })
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
/*
class ExecutorFut[T](factory: => T)(implicit context: ExecutionContext) extends Fut[T] {
  @volatile private var status: Option[T] = None
  private val monitor = new Object

  context.execute(() => {
    val result = factory
    monitor.synchronized {
      status = Some(result)
      monitor.notifyAll()
    }
  })

  override def await: T = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty)
        monitor.wait()
      status.get
    }
  }

  override def query: Option[T] =
    status
}

class SpatialFut[T](pos: V3F, factory: => T)(implicit executor: SpatialExecutor) extends Fut[T] {
  @volatile private var status: Option[T] = None
  private val monitor = new Object

  executor.execute(pos)(() => {
      val result = factory
      monitor.synchronized {
        status = Some(result)
        monitor.notifyAll()
      }
    })

  override def await: T = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty)
        monitor.wait()
      status.get
    }
  }

  override def query: Option[T] =
    status

}

class ImmediateFut[T](source: => T) extends Fut[T] {
  override lazy val await: T = source

  override def query: Option[T] = Some(await)

  override def map[E](func: (T) => E): Fut[E] = new ImmediateFut(func(await))
}

class CheapFutMap[S, R](source: Fut[S], func: S => R) extends Fut[R] {
  override lazy val await: R = func(source.await)

  override def query: Option[R] = source.query.map(func)
}

class CheapFutFlatMap[S, R](source: Fut[S], func: S => Fut[R]) extends Fut[R] {
  override lazy val await: R = func(source.await).await

  override def query: Option[R] = source.query.flatMap(func(_).query)
}
*/