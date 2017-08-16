package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.ThreadFactory

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}
import com.thesamet.spatial.KDTreeMap

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class SpatialExecutor(threads: Int, threadFactory: ThreadFactory) {

  @volatile private var tasks = KDTreeMap.empty[V3F, Runnable]
  private val taskMonitor = new Object

  var priorityPoint: V3F = Origin

  private def takeTask(): Runnable = {
    taskMonitor.synchronized {
      while (tasks isEmpty)
        taskMonitor.wait()
      val (pos, task) = tasks.findNearest(priorityPoint, 1).head
      tasks -= pos
      task
    }
  }

  def execute(pos: V3F, task: Runnable): Unit = {
    taskMonitor.synchronized {
      var insertPos = pos
      while (tasks contains insertPos)
        insertPos += V3F(0.1f, 0.1f, 0.1f)
      tasks += insertPos -> task
      taskMonitor.notifyAll()
    }
  }

  {
    val executionRoutine: Runnable = () => {
      while (true) takeTask().run()
    }
    for (_ <- 1 to threads) {
      threadFactory.newThread(executionRoutine).start()
    }
  }

}

object SpatialExecutor {

  implicit val global = new SpatialExecutor(Runtime.getRuntime.availableProcessors, runnable => {
    val thread = new Thread(runnable, "global spatial exec thread")
    thread.setPriority(3)
    thread
  })

}

trait Fut[T] {
  def await: T

  def query: Option[T]

  def cheapMap[E](func: T => E): Fut[E] = new CheapFutMap(this, func)

  def cheapFlatMap[E](func: T => Fut[E]): Fut[E] = new CheapFutFlatMap(this, func)
}

class FutureFut[T](future: Future[T]) extends Fut[T] {
  override def await: T = Await.result(future, Duration.Inf)

  override def query: Option[T] =
    if (future.isCompleted) Some(Await.result(future, Duration.Zero))
    else None
}

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

  executor.execute(pos, () => {
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

  override def cheapMap[E](func: (T) => E): Fut[E] = new ImmediateFut(func(await))
}

class CheapFutMap[S, R](source: Fut[S], func: S => R) extends Fut[R] {
  override def await: R = func(source.await)

  override def query: Option[R] = source.query.map(func)
}

class CheapFutFlatMap[S, R](source: Fut[S], func: S => Fut[R]) extends Fut[R] {
  override def await: R = func(source.await).await

  override def query: Option[R] = source.query.flatMap(func(_).query)
}