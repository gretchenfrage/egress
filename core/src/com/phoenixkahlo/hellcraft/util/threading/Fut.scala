package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.Executors
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.threading.MergeFut.{NoneCompleted, Status}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, parallel}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The fut system is an alternative to scala and java's future systems, that gives more flexibility to their
  * means of execution. Instead of the means of execution being hard-coded into the future system, the creation or
  * transformation of a <code>Fut</code> accepts a <code>Runnable => Unit</code> function that will be used to execute it.
  * This allows futs to be executed in a Java <code>Executor</code>, a Scala <code>ExecutionContext</code>, some
  * alternative such as a spatially prioritized executor, in a new thread, or even by hitching off of other threads using
  * <code>_.run()</code> as the executor (that is optimal for cheap transformations).
  */
trait Fut[+T] {
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

class FulfillmentContext[K, V] {
  private val lock = new ReentrantReadWriteLock
  private val map = new mutable.HashMap[K, Either[V, mutable.Buffer[V => Unit]]]

  def put(k: K, v: V): Unit = {
    lock.writeLock().lock()
    map.get(k) match {
      case Some(Right(waiting)) => waiting.foreach(_ apply v)
      case _ =>
    }
    map.put(k, Left(v))
    lock.writeLock.unlock()
  }

  def put(kvs: Seq[(K, V)]): Unit = {
    for ((k, v) <- kvs) put(k, v)
  }

  def remove(k: K): Unit = {
    lock.writeLock().lock()
    map.get(k) match {
      case Some(Left(_)) => map.remove(k)
      case _ => println("removed non-existant key from fulfillment context")
    }
    lock.writeLock().unlock()
  }

  def remove(ks: Iterable[K]): Unit = {
    for (k <- ks) {
      remove(k)
    }
  }

  def onFulfill(k: K, func: V => Unit): Unit = {
    lock.readLock.lock()
    map.get(k) match {
      case Some(Left(v)) =>
        func(v)
        lock.readLock.unlock()
      case _ =>
        lock.readLock.unlock()
        lock.writeLock().lock()
        map.get(k) match {
          case Some(Left(v)) => func(v)
          case Some(Right(buf)) => buf += func
          case None => map.put(k, Right(ArrayBuffer(func)))
        }
        lock.writeLock.unlock()
    }
  }
}
private class FulfillFut[K, V](k: K, context: FulfillmentContext[K, V]) extends Fut[V] {
  @volatile private var status: Option[V] = None
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  context.onFulfill(k, v => {
    monitor.synchronized {
      status = Some(v)
      monitor.notifyAll()
    }
    listeners.foreach(_.run())
  })

  override def await: V = {
    if (status isDefined) status.get
    else monitor.synchronized {
      while (status isEmpty) monitor.wait()
      status.get
    }
  }

  override def query: Option[V] =
    status

  override def onComplete(runnable: Runnable): Unit = {
    if (status isDefined) runnable.run()
    else monitor.synchronized {
      if (status isDefined) runnable.run()
      else listeners += runnable
    }
  }
}
object FulfillFut {
  def apply[K, V](key: K)(implicit context: FulfillmentContext[K, V]): Fut[V] =
    new FulfillFut(key, context)
}

object FulfillTest extends App {
  implicit val context = new FulfillmentContext[String, String]

  context.put("now", "now is now")

  val now = FulfillFut[String, String]("now")
  now.onComplete(() => println(now.query.get))

  val later = FulfillFut[String, String]("later")
  later.onComplete(() => println(later.query.get))

  new Thread(() => {
    Thread.sleep(1000)
    context.put("later", "later is now")
  }).start()


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

case class PromiseFold(sources: Seq[Fut[_]]) extends Fut[Unit] {
  private var waiting: Set[Int] = sources.indices.toSet
  private val waitingMonitor = new Object
  @volatile private var done = sources.isEmpty
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  for ((source, i) <- sources.zipWithIndex) {
    source.onComplete(() => {
      waitingMonitor.synchronized {
        waiting -= i
      }
      if (waiting.isEmpty) {
        monitor.synchronized {
          done = true
          monitor.notifyAll()
        }
        listeners.foreach(_.run())
      }
    })
  }

  override def await: Unit = monitor.synchronized {
    if (!done) monitor.synchronized {
      while (!done) monitor.wait()
    }
  }

  override def query: Option[Unit] = {
    if (done) Some((): Unit)
    else None
  }

  override def onComplete(runnable: Runnable): Unit = {
    if (done) runnable.run()
    else monitor.synchronized {
      if (done) runnable.run()
      else listeners += runnable
    }
  }
}
/*
object PromiseFoldTest extends App {
  val exec = Executors.newSingleThreadExecutor()
  val promises = new ArrayBuffer[Fut[Unit]]
  for (i <- 1 to 100){
    promises += Fut[Unit](println(i), exec.execute)
  }
  val fold = PromiseFold(promises)
  fold.onComplete(() => println("done!"))
}
*/
private class MergeFut[S1, S2, R](source1: Fut[S1], source2: Fut[S2], func: (S1, S2) => R, executor: Runnable => Unit) extends Fut[R] {
  private case class FirstCompleted(s1: S1) extends Status
  private case class SecondCompleted(s2: S2) extends Status
  private case class BothCompleted(r: R) extends Status

  @volatile private var status: Status = NoneCompleted
  private val listeners = new ArrayBuffer[Runnable]
  private val monitor = new Object

  source1.onComplete(() => {
    val s1 = source1.query.get
    monitor.synchronized {
      status match {
        case NoneCompleted =>
          status = FirstCompleted(s1)
        case SecondCompleted(s2) =>
          executor(() => {
            val result = func(s1, s2)
            monitor.synchronized {
              status = BothCompleted(result)
              monitor.notifyAll()
            }
            listeners.foreach(_.run())
          })
        case _ => ???
      }
    }
  })

  source2.onComplete(() => {
    val s2 = source2.query.get
    monitor.synchronized {
      status match {
        case NoneCompleted =>
          status = SecondCompleted(s2)
        case FirstCompleted(s1) =>
          executor(() => {
            val result = func(s1, s2)
            monitor.synchronized {
              status = BothCompleted(result)
              monitor.notifyAll()
            }
            listeners.foreach(_.run())
          })
        case _ => ???
      }
    }
  })

  override def await: R = {
    status match {
      case completed: BothCompleted => completed.r
      case _ => monitor.synchronized {
        while (!status.isInstanceOf[BothCompleted]) monitor.wait()
        status.asInstanceOf[BothCompleted].r
      }
    }
  }

  override def query: Option[R] = status match {
    case BothCompleted(r) => Some(r)
    case _ => None
  }

  override def onComplete(runnable: Runnable): Unit = {
    if (query isDefined) runnable.run()
    else monitor.synchronized {
      if (query isDefined) runnable.run()
      else listeners += runnable
    }
  }

}

/**
  * A Fut mapping where there are two inputs.
  */
object MergeFut {
  private sealed trait Status
  private case object NoneCompleted extends Status

  def apply[S1, S2, R](source1: Fut[S1], source2: Fut[S2], func: (S1, S2) => R)(implicit exec: Runnable => Unit): Fut[R] =
    new MergeFut(source1, source2, func, exec)

  def fold[T](futs: Seq[Fut[T]], start: T)(monoid: (T, T) => T)(implicit exec: Runnable => Unit) = {
    val startFut = Fut(start, _.run())
    futs.fold(startFut)(MergeFut(_, _, monoid))
  }
}
