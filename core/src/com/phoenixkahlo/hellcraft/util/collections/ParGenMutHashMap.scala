package com.phoenixkahlo.hellcraft.util.collections

import java.util.concurrent.locks.ReentrantReadWriteLock


import scala.collection.mutable

class ParGenMutHashMap[K, V](gen: K => V) {
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

  def remove(k: K): Unit = {
    lock.writeLock().lock()
    map.remove(k)
    lock.writeLock().unlock()
  }

  def toMap: Map[K, V] = {
    lock.readLock().lock()
    try map.toMap
    finally lock.readLock().unlock()
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

  def apply(k: K)(gen: => V = this.gen(k)): V = {
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
            val v = gen
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

  def getAndUpdate(k: K, trans: V => V): V = {
    lock.writeLock().lock()
    val v = map.getOrElse(k, gen(k))
    map.put(k, trans(v))
    lock.writeLock().unlock()
    v
  }

  def updateAndGet(k: K, trans: V => V): V = {
    lock.writeLock().lock()
    val v1 = map.getOrElse(k, gen(k))
    val v2 = trans(v1)
    map.put(k, v2)
    lock.writeLock().unlock()
    v2
  }

  def updateAndClean(k: K, trans: V => V): Unit = {
    lock.writeLock().lock()
    map.get(k) match {
      case Some(v1) =>
        val v2 = trans(v1)
        val vNew = gen(k)
        if (v2 == vNew)
          map.remove(k)
        else
          map.put(k, v2)
      case None =>
    }
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
