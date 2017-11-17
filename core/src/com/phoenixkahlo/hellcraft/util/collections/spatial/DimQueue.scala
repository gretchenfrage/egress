package com.phoenixkahlo.hellcraft.util.collections.spatial

import java.util
import java.util.PriorityQueue
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

import scala.collection.{JavaConverters, mutable}
import scala.collection.immutable.Queue

class SpatialPriorityQueue[K, E](empty: DimTree[Queue[E], _, K], spoint: K) extends java.util.AbstractQueue[(K, E)] {
  var point: K = spoint
  private var tree: DimTree[Queue[E], _, K] = empty
  private var _size: Int = 0

  override def poll(): (K, E) = {
    tree.closest(point, Float.MaxValue) match {
      case Some((key, queue)) =>
        val (value, newQueue) = queue.dequeue
        if (newQueue isEmpty) tree -= key
        else tree += key -> newQueue
        _size -= 1
        (key, value)
      case None => null
    }
  }

  def toSeq: Seq[(K, E)] = {
    tree.toSeq.flatMap({ case (k, queue) => queue.map(e => k -> e) })
  }

  override def offer(e: (K, E)): Boolean = {
    val (key, value) = e
    val queue = tree.getOrElse(key, Queue.empty)
    val newQueue = queue.enqueue(value)
    tree += key -> newQueue
    _size += 1
    true
  }

  override def peek(): (K, E) = {
    tree.closest(point, Float.MaxValue) match {
      case Some((k, q)) => k -> q.head
      case None => null
    }
  }

  override def iterator(): util.Iterator[(K, E)] =
    JavaConverters.asJavaIterator(tree.iterator.flatMap({ case (k, q) => q.map(k -> _) }))

  override def size(): Int = _size

  def height: Int = tree.height
}

class SpatialBlockingQueue[K, E](empty: DimTree[Queue[E], _, K], spoint: K) extends java.util.AbstractQueue[(K, E)] with util.concurrent.BlockingQueue[(K, E)] {
  private val queue = new SpatialPriorityQueue(empty, spoint)
  private val lock = new ReentrantReadWriteLock
  private val writeLock = lock.writeLock()
  private val readLock = lock.readLock()
  private val tickets = new LinkedBlockingQueue[AnyRef]
  private val ticket = new Object

  def point = {
    try {
      readLock.lock()
      queue.point
    } finally readLock.unlock()
  }

  def point_=(p: K) = {
    try {
      writeLock.lock()
      queue.point = p
    } finally writeLock.unlock()
  }

  override def poll(): (K, E) = {
    if (tickets.poll() == null) null
    else try {
      writeLock.lock()
      queue.remove()
    } finally writeLock.unlock()
  }

  override def poll(timeout: Long, unit: TimeUnit): (K, E) = poll()

  override def add(e: (K, E)): Boolean = {
    try {
      writeLock.lock()
      queue.add(e)
    } finally writeLock.unlock()
    tickets.add(ticket)
  }

  override def put(e: (K, E)): Unit = add(e)

  override def offer(e: (K, E)): Boolean = add(e)

  override def offer(e: (K, E), timeout: Long, unit: TimeUnit): Boolean = add(e)

  override def drainTo(c: util.Collection[_ >: (K, E)], maxElements: Int): Int = {
    try {
      readLock.lock()
      var item: (K, E) = null
      var count = 0
      while (count < maxElements && {item = poll(); item} != null) {
        c.add(item)
        count += 1
      }
      count
    } finally readLock.unlock()
  }

  override def drainTo(c: util.Collection[_ >: (K, E)]): Int = drainTo(c, Int.MaxValue)

  override def take(): (K, E) = {
    tickets.take()
    try {
      writeLock.lock()
      queue.remove()
    } finally writeLock.unlock()
  }

  override def remainingCapacity(): Int = Int.MaxValue

  override def peek(): (K, E) = {
    try {
      readLock.lock()
      queue.peek()
    } finally readLock.unlock()
  }

  override def iterator(): util.Iterator[(K, E)] = {
    try {
      readLock.lock()
      queue.iterator()
    } finally readLock.unlock()
  }

  override def size(): Int = {
    try {
      readLock.lock()
      queue.size()
    } finally readLock.unlock()
  }
}

object OctreeQueueTest extends App {
  val queue = new SpatialBlockingQueue[V3F, Int](Octree.empty(Origin, Float.MaxValue), Origin)

  new Thread(new Runnable {
    override def run() = while (true) {
      println(queue.take())
    }
  }).start()

  new Thread(new Runnable {
    override def run(): Unit = while (true) {
      Thread.sleep(1000)
      for (n <- (1 to 100)) {
        queue.add(V3F(n, n, n) -> n)
      }
    }
  }).start()
}