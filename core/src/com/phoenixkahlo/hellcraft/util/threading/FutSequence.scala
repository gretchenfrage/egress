package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.locks.ReentrantReadWriteLock


/**
  * Utility for executing futs sequentially.
  */
class FutSequence(executor: Runnable => Unit) {

  private val lock = new ReentrantReadWriteLock
  private var last: Fut[_] = Fut[Unit]((), _.run())

  def apply[T](factory: () => T): Fut[T] = {
    /*
    if (!lock.writeLock.tryLock()) {
      println("must wait for lock")
      lock.writeLock.lock()
    }
    */
    lock.writeLock().lock()
    val next = last.afterwards(factory, executor)
    last = next
    lock.writeLock().unlock()
    next
  }

  def flatChain[T](factory: => Fut[T]): Fut[T] = {
    lock.writeLock().lock()
    val next = last.flatMap(any => factory)
    last = next
    lock.writeLock().unlock()
    next
  }

  def getLast: Fut[_] = {
    lock.readLock.lock()
    try last
    finally lock.readLock().unlock()
  }

}

class FutChain[T](start: T, exec: Runnable => Unit) {
  @volatile private var _curr: Fut[T] = Fut(start, exec)

  def update(func: T => T): Fut[T] = this.synchronized {
    _curr = _curr.map(func, exec)
    _curr
  }

  def flatUpdate[F <: Fut[T]](func: T => F): F = this.synchronized {
    _curr = _curr.flatMap(func)
    _curr.asInstanceOf[F]
  }

  def getAndSet[F <: Fut[T]](fut: F): (Fut[T], F) = this.synchronized {
    val before = _curr
    _curr = fut
    (before, _curr.asInstanceOf[F])
  }

  def curr: Fut[T] = _curr
}