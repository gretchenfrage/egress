package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.locks.ReentrantReadWriteLock

class Domain[D, O](func: D => O) extends (D => Option[O]) {
  private val lock = new ReentrantReadWriteLock
  @volatile private var _domain: Set[D] = Set.empty
  private var map: Map[D, O] = Map.empty

  def domain: Set[D] = _domain
  def domain_=(newDomain: Set[D]): Unit = {
    lock.writeLock().lock()

    val oldDomain = _domain
    _domain = newDomain

    val removed = oldDomain -- newDomain
    map --= removed

    val added = newDomain -- oldDomain
    for (d <- added) {
      map += (d -> func(d))
    }

    lock.writeLock().unlock()
  }

  override def apply(d: D): Option[O] = {
    try {
      lock.readLock().lock()
      map.get(d)
    } finally lock.readLock().unlock()
  }
}
