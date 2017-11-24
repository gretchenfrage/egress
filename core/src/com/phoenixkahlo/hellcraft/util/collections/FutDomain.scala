package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.locks.ReentrantReadWriteLock

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
}