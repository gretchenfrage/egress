package com.phoenixkahlo.hellcraft.threading

class SeqFutFactory(executor: Runnable => Unit) {

  private var last: Fut[_] = Fut[Unit]((), _.run())

  def apply[T](factory: => T): Fut[T] = this.synchronized {
    val next = last.afterwards(factory, executor)
    last = next
    next
  }

}
