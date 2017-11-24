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