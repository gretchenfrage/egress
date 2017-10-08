package com.phoenixkahlo.hellcraft.util.spatial

import java.util
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

import scala.collection.JavaConverters
import scala.collection.immutable.Queue

class SpatialHashMapQueue[E](binSize: Float) extends util.AbstractQueue[(V3F, E)] {

  var point: V3F = Origin
  var map: SpatialHashMap[E] = SpatialHashMap(binSize)

  override def poll(): (V3F, E) = {
    map.closest(point) match {
      case Some((k, v)) =>
        map -= k
        (k, v)
      case None => null
    }
  }

  override def offer(e: (V3F, E)): Boolean = {
    map += e
    true
  }

  override def peek(): (V3F, E) = {
    map.closest(point) match {
      case Some(item) => item
      case None => null
    }
  }

  override def iterator(): util.Iterator[(V3F, E)] =
    JavaConverters.asJavaIterator(map.iterator)

  override def size(): Int =
    map.size

}

class SpatialHashMapBinPriorityQueue[E](binSize: Float) extends util.AbstractQueue[(V3F, E)] {

  var point: V3F = Origin
  var map: SpatialHashMap[Queue[E]] = SpatialHashMap(binSize)

  override def poll(): (V3F, E) = {
    map.closest(point) match {
      case Some((key, queue)) =>
        val (value, newQueue) = queue.dequeue
        if (newQueue isEmpty) map -= key
        else map += key -> newQueue
        _size -= 1
        (key, value)
      case None => null
    }
  }

  override def offer(e: (V3F, E)): Boolean = {
    val (key, value) = e
    val queue = map.getOrElse(key, Queue.empty)
    val newQueue = queue.enqueue(value)
    map += key -> newQueue
    _size += 1
    true
  }

  override def peek(): (V3F, E) = {
    map.closest(point) match {
      case Some((k, q)) => k -> q.head
      case None => null
    }
  }

  override def iterator(): util.Iterator[(V3F, E)] =
    JavaConverters.asJavaIterator(map.iterator.flatMap({ case (k, q) => q.map(k -> _) }))

  @volatile private var _size: Int = 0

  override def size(): Int = _size

}

class SpatialHashMapBlockingQueue[E](binSize: Float) extends util.AbstractQueue[(V3F, E)] with util.concurrent.BlockingQueue[(V3F, E)] {
  private val queue = new SpatialHashMapBinPriorityQueue[E](binSize)
  private val lock = new ReentrantReadWriteLock
  private val writeLock = lock.writeLock()
  private val readLock = lock.readLock()
  private object Ticket
  private val tickets = new LinkedBlockingQueue[Object]

  def point = {
    try {
      readLock.lock()
      queue.point
    } finally readLock.unlock()
  }

  def point_=(p: V3F) = {
    try {
      writeLock.lock()
      queue.point = p
    } finally writeLock.unlock()
  }

  override def poll(): (V3F, E) = {
    if (tickets.poll() == null) null
    else try {
      writeLock.lock()
      queue.remove()
    } finally writeLock.unlock()
  }

  override def poll(timeout: Long, unit: TimeUnit): (V3F, E) = this.poll()

  override def add(e: (V3F, E)): Boolean = {
    try {
      writeLock.lock()
      queue.add(e)
    } finally writeLock.unlock()
    tickets.add(Ticket)
  }

  override def put(e: (V3F, E)): Unit = this.add(e)

  override def offer(e: (V3F, E)): Boolean = this.add(e)

  override def offer(e: (V3F, E), timeout: Long, unit: TimeUnit): Boolean = this.add(e)

  override def drainTo(c: util.Collection[_ >: (V3F, E)], maxElements: Int): Int = {
    try {
      readLock.lock()
      var item: (V3F, E) = null
      var count = 0
      while (count < maxElements && {item = poll(); item} != null) {
        c.add(item)
        count += 1
      }
      count
    } finally readLock.unlock()
  }

  override def drainTo(c: util.Collection[_ >: (V3F, E)]): Int = this.drainTo(c, Int.MaxValue)

  override def take(): (V3F, E) = {
    tickets.take()
    try {
      writeLock.lock()
      queue.remove()
    } finally writeLock.unlock()
  }

  override def remainingCapacity(): Int = Int.MaxValue

  override def peek(): (V3F, E) = {
    try {
      readLock.lock()
      queue.peek()
    } finally readLock.unlock()
  }

  /**
    * Since the OctreeBlockingQueue is backed by an OctreePriorityQueue, which is backed by an immutable Octree
    * data structure, iterating over this queue actually is a thread safe operation which will iterate over
    * the version of the items in the queue at the time the iterator was produced.
    */
  override def iterator(): util.Iterator[(V3F, E)] = {
    try {
      readLock.lock()
      queue.iterator()
    } finally readLock.unlock()
  }

  override def size(): Int = {
    try {
      readLock.lock()
      queue.size
    } finally readLock.unlock()
  }
}

object SpatialHashMapQueueTest extends App {

  val queue = new SpatialHashMapBlockingQueue[Int](25)
  queue.point = V3F(101, 101, 101)

  new Thread(() => {
    while (true) {
      println(queue.take())
    }
  }).start()

  new Thread(() => {
    while (true) {
      Thread.sleep(1000)
      for (n <- (1 to 100).reverse) {
        queue.add((V3F(n, n, n), n))
      }
    }
  }).start()

}