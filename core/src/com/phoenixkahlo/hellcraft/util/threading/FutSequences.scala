package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.locks.ReentrantReadWriteLock


/**
  * Utility for executing futs sequentially.
  */
class FutSequences(executor: Runnable => Unit) {

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

  def getLast: Fut[_] = {
    lock.readLock.lock()
    try last
    finally lock.writeLock().unlock()
  }

}
