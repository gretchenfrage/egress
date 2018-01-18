package com.phoenixkahlo.hellcraft.util.collections.queue

trait Queue[T] {
  def out: QueueOut[T]
  def in: QueueIn[T]
}

trait Deque[T] extends Queue[T] {
  def front: QueueIn[T]
}

trait QueueOut[+T] {
  def poll(): T

  def map[E](func: T => E): QueueOut[E] = new QueueOutMap(this, func)
}

trait QueueIn[-T] {
  def push(t: T): Unit

  def map[E](func: E => T): QueueIn[E] = new QueueInMap(this, func)
}

private class QueueOutMap[T, E](src: QueueOut[T], func: T => E) extends QueueOut[E] {
  override def poll(): E = src.poll() match {
    case null => null.asInstanceOf[E]
    case some => func(some)
  }
}

private class QueueInMap[T, E](src: QueueIn[T], func: E => T) extends QueueIn[E] {
  override def push(t: E): Unit = src.push(func(t))
}