package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.util.threading.{AlwaysCancelled, CancellableFut, Fut}

class FutDomain[D, O](func: D => CancellableFut[O]) extends (D => CancellableFut[O]) {
  private val lock = new ReentrantReadWriteLock
  @volatile private var _domain: Set[D] = Set.empty
  private var map: Map[D, CancellableFut[O]] = Map.empty

  def domain: Set[D] = _domain
  def domain_=(newDomain: Set[D]): Unit = {
    lock.writeLock().lock()

    val oldDomain = _domain
    _domain = newDomain

    val removed = oldDomain -- newDomain
    for (d <- removed) {
      map(d).cancel()
    }
    map --= removed

    val added = newDomain -- oldDomain
    for (d <- added) {
      map += (d -> func(d))
    }

    lock.writeLock().unlock()
  }

  override def apply(d: D): CancellableFut[O] = {
    lock.readLock.lock()
    val f = map.get(d)
    lock.readLock().unlock()
    f.getOrElse({
      println("warning: fut domain queried for fut outside of domain")
      AlwaysCancelled
    })
  }
}
//class FutDomain[D, O](func: D => O, exec: D => (Runnable => Unit)) extends (D => Fut[Option[O]])
/*
import com.phoenixkahlo.hellcraft.util.threading.{Fut, Scheduler, WeakFut, WeakHandle}

import scala.concurrent.duration.Duration

class FutDomain[D, O](func: D => O, exec: D => (Runnable => Unit)) extends (D => Fut[Option[O]]) {
  private val lock = new ReentrantReadWriteLock
  private var map: Map[D, (WeakFut[O], WeakHandle[O])] = Map.empty

  def setDomain(newDomain: Set[D]): Unit = {
    lock.writeLock().lock()

    val oldDomain = map.keySet

    val removed = oldDomain -- newDomain
    for (d <- removed) {
      map(d)._1.cancel()
      map -= d
    }

    val added = newDomain -- oldDomain
    for (d <- added) {
      map += (d -> WeakFut(func(d), exec(d)))
    }

    lock.writeLock().unlock()
  }

  override def apply(d: D): Fut[Option[O]] = {
    lock.readLock().lock()
    val f = map.get(d).map(_._1)
    lock.readLock().unlock()
    f.getOrElse({
      println("warning: fut domain queried for fut outside of domain")
      Fut(None, _.run())
    })
  }
}*/