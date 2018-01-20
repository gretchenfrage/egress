package com.phoenixkahlo.hellcraft.util.retro

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.phoenixkahlo.hellcraft.util.retro.Retro.{Exec, Invalidator, Observer, Version}
import com.phoenixkahlo.hellcraft.util.threading.Fut

import scala.collection.mutable

trait Retro[+T] {
  def hardAwait: T
  def softAwait: T
  def hardQuery: Option[T]
  def softQuery: Option[T]

  def map[R](func: T => R, exec: Exec): Retro[R] = new MapRetro(this, func, exec)
  def flatMap[R](func: T => Retro[R]): Retro[R] = new FlatMapRetro(this, func)

  def observe[O >: T](observer: Observer[O], invalidator: Invalidator): Unit

  def flatten[R](implicit asRetro: T => Retro[R]): Retro[R] = flatMap(asRetro)
}

object Retro {
  type Exec = Runnable => Unit
  type Version = Vector[Int]
  type Observer[-T] = (T, Version) => Unit // the observer also works to invalidate
  type Invalidator = Version => Unit

  def apply[T](gen: => T, exec: Exec): Retro[T] = new GenRetro[T](gen, exec)
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
    status = Some(value)
    this.notifyAll()
    for (observer <- observers) {
      observer.apply(value, Vector(version))
    }
    version += 1
  }

  override def hardAwait: T = {
    if (status isDefined) status.get
    else this.synchronized {
      while (status isEmpty)
        this.wait()
      status.get
    }
  }

  override def softAwait: T = softAwait

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