package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.locks.ReentrantReadWriteLock

import scala.collection.mutable

class ParGenMutHashMap[K, V](gen: K => V) extends {
  private val map = new mutable.HashMap[K, V]
  private val lock = new ReentrantReadWriteLock()

  def toSeq: Seq[(K, V)] = {
    lock.writeLock().lock()
    try map.toSeq
    finally lock.writeLock().unlock()
  }

  def put(k: K, v: V): Unit = {
    lock.writeLock().lock()
    map.put(k, v)
    lock.writeLock().unlock()
  }

  def putUniqueKey(keygen: => K, v: V): K = {
    lock.writeLock().lock()
    var k = keygen
    while (map contains k) {
      k = keygen
    }
    map.put(k, v)
    lock.writeLock().unlock()
    k
  }

  def apply(k: K): V = {
    lock.readLock().lock()
    map.get(k) match {
      case Some(v) =>
        lock.readLock().unlock()
        v
      case None =>
        lock.readLock().unlock()
        lock.writeLock().lock()
        map.get(k) match {
          case Some(v) =>
            lock.writeLock().unlock()
            v
          case None =>
            val v = gen(k)
            map.put(k, v)
            lock.writeLock().unlock()
            v
        }
    }
  }

  def update(k: K, trans: V => V): Unit = {
    lock.writeLock().lock()
    map.put(k, trans(map.getOrElse(k, gen(k))))
    lock.writeLock().unlock()
  }

  def clean(): Unit = {
    lock.writeLock().lock()
    for ((k, v) <- map) {
      if (v == gen(k))
        map.remove(k)
    }
    lock.writeLock().unlock()
  }

}
