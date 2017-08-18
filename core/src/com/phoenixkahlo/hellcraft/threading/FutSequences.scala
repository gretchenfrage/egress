package com.phoenixkahlo.hellcraft.threading

/**
  * Utility for executing futs sequentially.
  */
class FutSequences(executor: Runnable => Unit) {

  private var last: Fut[_] = Fut[Unit]((), _.run())

  def apply[T](factory: => T): Fut[T] = this.synchronized {
    val next = last.afterwards(factory, executor)
    last = next
    next
  }

}
