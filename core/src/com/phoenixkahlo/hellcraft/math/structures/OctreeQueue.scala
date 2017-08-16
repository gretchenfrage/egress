package com.phoenixkahlo.hellcraft.math.structures

import java.util
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import com.phoenixkahlo.hellcraft.math.{Origin, V3F}

import scala.collection.JavaConverters
import scala.collection.immutable.Queue

/**
  * Uses an octree to create a priority queue based of vector/generic pairs based on their proximity to an adjustable
  * point. This is not thread safe.
  */
class OctreePriorityQueue[E] extends util.AbstractQueue[(V3F, E)] {

  var point: V3F = Origin
  var tree: Octree[E] = EmptyOctree(Origin, Float.MaxValue)

  override def poll(): (V3F, E) = {
    tree.closest(point) match {
      case Some((k, v)) =>
        tree -= k
        (k, v)
      case None => null
    }
  }

  override def offer(e: (V3F, E)): Boolean = {
    tree += e
    true
  }

  override def peek(): (V3F, E) = {
    tree.closest(point) match {
      case Some(item) => item
      case None => null
    }
  }

  override def iterator(): util.Iterator[(V3F, E)] =
    JavaConverters.asJavaIterator(tree.iterator)

  override def size(): Int =
    tree.size

}

class OctreeBinPriorityQueue[E] extends util.AbstractQueue[(V3F, E)] {

  var point: V3F = Origin
  var tree: Octree[Queue[E]] = EmptyOctree(Origin, Float.MaxValue)

  override def poll(): (V3F, E) = {
    tree.closest(point) match {
      case Some((key, queue)) =>
        val (value, newQueue) = queue.dequeue
        if (newQueue isEmpty) tree -= key
        else tree += key -> newQueue
        (key, value)
      case None => null
    }
  }

  override def offer(e: (V3F, E)): Boolean = {
    val (key, value) = e
    val queue = tree.getOrElse(key, Queue.empty)
    val newQueue = queue.enqueue(value)
    tree += key -> newQueue
    true
  }

  override def peek(): (V3F, E) = {
    tree.closest(point) match {
      case Some((k, q)) => k -> q.head
      case None => null
    }
  }

  override def iterator(): util.Iterator[(V3F, E)] =
    JavaConverters.asJavaIterator(tree.iterator.flatMap({ case (k, q) => q.map(k -> _) }))

  override def size(): Int =
    tree.values.map(_.size).sum
}

/**
  * Implements a concurrency layer on top of an OctreePriorityQueue to make it a BlockingQueue.
  *
  * This is implemented using a ReentrantReadWriteLock, for reading and writing to the internal queue, and a
  * LinkedBlockingQueue of "tickets", which only holds references to a "ticket" singleton. Whenever a thread adds an
  * item to the queue, it then adds a ticket to the ticket queue. Whenever a thread wishes to remove an item from
  * the queue, it first takes a ticket from the ticket queue. The reading and writing of the wrapped queue
  * is still protected by the lock, but the ticket taking and giving is not. The purpose of this system is to
  * delegate the blocking logic a blocking system which is already established to work efficiently.
  */
class OctreeBlockingQueue[E] extends util.AbstractQueue[(V3F, E)] with util.concurrent.BlockingQueue[(V3F, E)] {
  private val queue = new OctreeBinPriorityQueue[E]
  private val lock = new ReentrantReadWriteLock
  private val writeLock = lock.writeLock()
  private val readLock = lock.readLock()
  private object Ticket
  private val tickets = new LinkedBlockingQueue[Object]

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
      queue.size()
    } finally readLock.unlock()
  }
}

object OctreeQueueTest extends App {

  val queue = new OctreeBlockingQueue[Int]

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