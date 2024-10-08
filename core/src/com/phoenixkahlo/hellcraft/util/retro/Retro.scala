package com.phoenixkahlo.hellcraft.util.retro

import java.util
import java.util.Collections
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.retro.Retro.{Exec, Invalidator, Observer, Version}
import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.{JavaConverters, mutable}

/**
  * The retroactively changing monad.
  */
trait Retro[+T] {
  def hardAwait: T
  def softAwait: T
  def hardQuery: Option[T]
  def softQuery: Option[T]

  def map[R](func: T => R, exec: Exec): Retro[R] = new MapRetro(this, func, exec)
  def flatMap[R](func: T => Retro[R]): Retro[R] = new FlatMapRetro(this, func)


  def observe[O >: T](observer: Observer[O], invalidator: Invalidator): Unit

  def flatten[R](implicit asRetro: T => Retro[R]): Retro[R] = flatMap(asRetro)
  def filter(test: T => Boolean): Retro[T] = this // ignore filters
}

object Retro {
  type Exec = Runnable => Unit
  type Version = Vector[Int]
  type Observer[-T] = (T, Version) => Unit // the observer also works to invalidate
  type Invalidator = Version => Unit

  def apply[T](gen: => T, exec: Exec): Retro[T] = new GenRetro[T](gen, exec)

  def fromFut[T](fut: Fut[T]): Retro[T] = {
    val r = new SetRetro[T]
    fut.onComplete(() => r.set(fut.query.get))
    r
  }
}

private class GenRetro[T](gen: => T, exec: Exec) extends Retro[T] {
  @volatile private var status: Option[T] = None
  private val observers = new mutable.ArrayBuffer[Observer[T]]

  exec(() => {
    val result = gen
    GenRetro.this.synchronized {
      status = Some(result)
      GenRetro.this.notifyAll()
      for (observer <- observers)
        observer(result, GenRetro.version)
    }
  })

  override def hardAwait: T = {
    if (status isDefined) status.get
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get
    }
  }

  override def softAwait: T = hardAwait

  override def hardQuery: Option[T] = status

  override def softQuery: Option[T] = status

  def observe[O >: T](observer: Observer[O], invalidator: Invalidator): Unit = this.synchronized {
    if (status isDefined)
      observer(status.get, GenRetro.version)
    observers += observer
  }
}
private object GenRetro {
  val version: Version = Vector(1)
}

class SetRetro[T] extends Retro[T] {
  @volatile private var status: Option[T] = None
  private var version = 0
  private val observers = new mutable.ArrayBuffer[Observer[T]]

  def set(value: T): Unit = this.synchronized {
    if (!status.contains(value)) {
      status = Some(value)
      this.notifyAll()
      for (observer <- observers) {
        observer.apply(value, Vector(version))
      }
      version += 1
    }
  }

  override def hardAwait: T = {
    if (status isDefined) status.get
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get
    }
  }

  override def softAwait: T = hardAwait

  override def hardQuery: Option[T] = status

  override def softQuery: Option[T] = status

  def observe[O >: T](observer: Observer[O], invalidator: Invalidator): Unit = this.synchronized {
    if (status isDefined)
      observer(status.get, Vector(version))
    observers += observer
  }
}

private class MapRetro[S, R](src: Retro[S], func: S => R, exec: Exec) extends Retro[R] {
  @volatile private var status: Option[(R, Version)] = None
  @volatile private var latest: Version = Vector.empty
  private val observers = new mutable.ArrayBuffer[Observer[R]]
  private val invalidators = new mutable.ArrayBuffer[Invalidator]

  src.observe((s, version) => {
    latest = version
    for (invalidator <- invalidators)
      invalidator(version)
    exec(() => {
      val result = func(s)
      MapRetro.this.synchronized {
        if (latest == version) {
          status = Some((result, version))
          MapRetro.this.notifyAll()
          for (observer <- observers)
            observer(result, version)
        }
      }
    })
  }, version => {
    latest = version
  })

  override def hardAwait: R = {
    status match {
      case Some((r, v)) if v == latest => r
      case _ => this.synchronized {
        while (!status.exists(_._2 == latest))
          this.wait()
        status.get._1
      }
    }
  }

  override def softAwait: R = {
    if (status isDefined) status.get._1
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get._1
    }
  }

  override def hardQuery: Option[R] = status.filter(_._2 == latest).map(_._1)

  override def softQuery: Option[R] = status.map(_._1)

  def observe[O >: R](observer: Observer[O], invalidator: Invalidator): Unit = this.synchronized {
    if (status.exists(_._2 == latest)) {
      val (r, v) = status.get
      observer(r, v)
    }
    observers += observer
    invalidators += invalidator
  }
}

private class FlatMapRetro[S, R](src: Retro[S], func: S => Retro[R]) extends Retro[R] {
  @volatile private var status: Option[(R, Version, Version)] = None
  @volatile private var latest: (Version, Version) = (Vector.empty, Vector.empty)
  private val observers = new mutable.ArrayBuffer[Observer[R]]
  private val invalidators = new mutable.ArrayBuffer[Invalidator]

  src.observe((s, v1) => {
    FlatMapRetro.this.synchronized {
      latest = (v1, latest._2)
      val v = latest._1 ++ latest._2
      for (invalidator <- invalidators)
        invalidator(v)
    }
    func(s).observe((r, v2) => {
      FlatMapRetro.this.synchronized {
        if (latest._1 == v1) {
          latest = (v1, v2)
          status = Some((r, v1, v2))
          FlatMapRetro.this.notifyAll()
          val v = latest._1 ++ latest._2
          for (observer <- observers)
            observer(r, v)
        }
      }
    }, v2 => {
      FlatMapRetro.this.synchronized {
        latest = (latest._1, v2)
        val v = latest._1 ++ latest._2
        for (invalidator <- invalidators)
          invalidator(v)
      }
    })
  }, v1 => {
    FlatMapRetro.this.synchronized {
      latest = (v1, latest._2)
      val v = latest._1 ++ latest._2
      for (invalidator <- invalidators)
        invalidator(v)
    }
  })

  override def hardAwait: R = {
    status match {
      case Some((r, v1, v2)) if (v1, v2) == latest => r
      case _ =>  this.synchronized {
        while (!status.exists { case (r, v1, v2) => (v1, v2) == latest })
          this.wait()
        status.get._1
      }
    }
  }

  override def softAwait: R = {
    if (status isDefined) status.get._1
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get._1
    }
  }

  override def hardQuery: Option[R] = status.filter({ case (r, v1, v2) => (v1, v2) == latest }).map(_._1)

  override def softQuery: Option[R] = status.map(_._1)

  def observe[O >: R](observer: Observer[O], invalidator: Invalidator): Unit = this.synchronized {
    if (status.exists({ case (r, v1, v2) => (v1, v2) == latest }))
      observer(status.get._1, status.get._2 ++ status.get._3)
    observers += observer
    invalidators += invalidator
  }
}

class WeaklyObservableRetro[T](src: Retro[T]) extends Retro[T] {
  private val lock = new ReentrantReadWriteLock()
  private val set: java.util.Set[(Observer[T], Invalidator)] =
    Collections.newSetFromMap(new util.WeakHashMap)
  private var curr: Option[T] = None
  private var currVersion: Version = Vector.empty

  src.observe((r, v) => {
    lock.readLock.lock()
    for ((observer, _) <- JavaConverters.collectionAsScalaIterable(set))
      observer(r, v)
    curr = Some(r)
    currVersion = v
    lock.readLock.unlock()
  }, v => {
    lock.readLock.lock()
    for ((_, invalidator) <- JavaConverters.collectionAsScalaIterable(set))
      invalidator(v)
    lock.readLock.unlock()
  })

  override def hardAwait: T = src.hardAwait
  override def softAwait: T = src.softAwait
  override def hardQuery: Option[T] = src.hardQuery
  override def softQuery: Option[T] = src.softQuery

  override def observe[O >: T](observer: Observer[O], invalidator: Invalidator): Unit = {
    lock.writeLock.lock()
    set.add((observer, invalidator))
    if (curr isDefined)
      observer(curr.get, currVersion)
    lock.writeLock.unlock()
  }
}

class DisposalMutex {
  private val rrwl = new ReentrantReadWriteLock
  def acquire(): Unit = rrwl.readLock().lock()
  def release(): Unit = rrwl.readLock().unlock()
  def prevent(): Unit = rrwl.writeLock().lock()
  def allow(): Unit = rrwl.writeLock().unlock()
}

class DisposingMapRetro[S, R](src: Retro[S], func: S => R, disposer: R => Unit, exec: Exec, mutex: Option[DisposalMutex] = None) extends Retro[R] {
  @volatile private var status: Option[(R, Version)] = None
  @volatile private var latest: Version = Vector.empty
  private val observers = new mutable.ArrayBuffer[Observer[R]]
  private val invalidators = new mutable.ArrayBuffer[Invalidator]

  src.observe((s, version) => {
    latest = version
    for (invalidator <- invalidators)
      invalidator(version)
    exec(() => {
      val result: R = func(s)
      mutex.foreach(_.acquire())
      DisposingMapRetro.this.synchronized {
        if (latest == version) {
          //status.map(_._1).foreach(disposer)
          val before = status.map(_._1)
          status = Some((result, version))
          before.foreach(disposer)
          DisposingMapRetro.this.notifyAll()
          for (observer <- observers)
            observer(result, version)
        } else disposer(result)
      }
      mutex.foreach(_.release())
    })
  }, version => {
    latest = version
  })

  def dispose(): Unit = this.synchronized {
    mutex.foreach(_.acquire())
    //status.map(_._1).foreach(disposer)
    val before = status.map(_._1)
    status = None
    before.foreach(disposer)
    latest = Vector.empty
    mutex.foreach(_.release())
  }

  override def hardAwait: R = {
    status match {
      case Some((r, v)) if v == latest => r
      case _ => this.synchronized {
        while (!status.exists(_._2 == latest))
          this.wait()
        status.get._1
      }
    }
  }

  override def softAwait: R = {
    if (status isDefined) status.get._1
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get._1
    }
  }

  override def hardQuery: Option[R] = status.filter(_._2 == latest).map(_._1)

  override def softQuery: Option[R] = status.map(_._1)

  def observe[O >: R](observer: Observer[O], invalidator: Invalidator): Unit = this.synchronized {
    if (status.exists(_._2 == latest)) {
      val (r, v) = status.get
      observer(r, v)
    }
    observers += observer
    invalidators += invalidator
  }
}