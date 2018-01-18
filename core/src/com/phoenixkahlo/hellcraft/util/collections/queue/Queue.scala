package com.phoenixkahlo.hellcraft.util.collections.queue

trait Queue[T] {
  def out: QueueOut[T]
  def in: QueueIn[T]
}

trait Deque[T] extends Queue[T] {
  def front: QueueIn[T]
}

trait QueueOut[+T] extends (() => T) {
  def poll(): T

  override def apply(): T = poll()

  def map[E](func: T => E): QueueOut[E] = new QueueOutMap(this, func)

  def drainTo(consumer: T => Unit): Unit = {
    var t: T = null.asInstanceOf[T]
    while ({t = poll(); t != null})
      consumer(t)
  }
}

trait QueueIn[-T] extends (T => Unit) {
  def push(t: T): Unit

  override def apply(t: T): Unit = push(t)

  def map[E](func: E => T): QueueIn[E] = new QueueInMap(this, func)
}

private class QueueOutMap[T, E](src: QueueOut[T], func: T => E) extends QueueOut[E] {
  override def poll(): E = src.poll() match {
    case null => null.asInstanceOf[E]
    case some => func(some)
  }

  override def drainTo(consumer: (E) => Unit): Unit =
    src.drainTo(func andThen consumer)
}

private class QueueInMap[T, E](src: QueueIn[T], func: E => T) extends QueueIn[E] {
  override def push(t: E): Unit = src.push(func(t))
}