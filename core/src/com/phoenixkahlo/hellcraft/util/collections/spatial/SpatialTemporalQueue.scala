package com.phoenixkahlo.hellcraft.util.collections.spatial

import java.util
import java.util.PriorityQueue
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.math.{Origin, V2F, V3F, V4F}

import scala.collection.immutable.Queue

class SpatialTemporalQueue3D[E](timeToSpace: Long => Float) extends SpatialTemporalQueue[V3F, V4F, E](timeToSpace) {
  override protected def emptyTree = HexadecaTree.empty(V4F(0, 0, 0, 0), Float.MaxValue)

  override protected def startPoint = Origin

  override protected def inflate(k: V3F, s: Float) = k.inflate(s)

  override protected def flatten(h: V4F) = h.flatten
}

class SpatialTemporalQueue2D[E](timeToSpace: Long => Float) extends SpatialTemporalQueue[V2F, V3F, E](timeToSpace) {
  override protected def emptyTree = Octree.empty(Origin, Float.MaxValue)

  override protected def startPoint = V2F(0, 0)

  override protected def inflate(k: V2F, s: Float) = k.inflate(s)

  override protected def flatten(h: V3F) = h.flatten
}

object SpatialTemporalQueue {
  val secondEqualsMeter: Long => Float = _ / 1000000000f
}

abstract class SpatialTemporalQueue[K, H, E](timeToSpace: Long => Float) extends util.AbstractQueue[(K, E)]
    with util.concurrent.BlockingQueue[(K, E)] {

  private val startTime = System.nanoTime()

  protected def emptyTree: DimTree[Queue[E], _, H]
  protected def startPoint: K
  protected def inflate(k: K, s: Float): H
  protected def flatten(h: H): K

  private def inflate(kv: (K, E), s: Float): (H, E) = (inflate(kv._1, s), kv._2)
  private def flatten(kv: (H, E)): (K, E) = (flatten(kv._1), kv._2)

  private def now(kv: (K, E)): (H, E) = inflate(kv, timeToSpace(System.nanoTime() - startTime))

  private val queue = new SpatialPriorityQueue(emptyTree, inflate(startPoint, timeToSpace(startTime)))
  private val minTime = new PriorityQueue[Float]
  private val lock = new ReentrantReadWriteLock
  private val writeLock = lock.writeLock()
  private val readLock = lock.readLock()
  private object Ticket
  private val tickets = new LinkedBlockingQueue[AnyRef]

  def point: K = {
    try {
      readLock.lock()
      flatten(queue.point)
    } finally readLock.unlock()
  }

  def point_=(p: K): Unit = {
    try {
      writeLock.lock()
      queue.point = inflate(p, minTime.peek())
    } finally writeLock.unlock()
  }

  /**
    * This function does not itself lock anything.
    * You should only need to retime after removing a value, not after adding one.
    */
  private def retime(): Unit = {
    queue.point = inflate(flatten(queue.point), minTime.peek())
  }

  override def poll(): (K, E) = {
    if (tickets.poll() == null) null
    else try {
      writeLock.lock()
      flatten(queue.remove())
    } finally {
      retime()
      writeLock.unlock()
    }
  }

  override def poll(timeout: Long, unit: TimeUnit) = poll()

  override def add(e: (K, E)) = {
    try {
      writeLock.lock()
      queue.add(now(e))
    } finally writeLock.unlock()
    tickets.add(Ticket)
  }

  override def put(e: (K, E)) = add(e)

  override def offer(e: (K, E)) = add(e)

  override def offer(e: (K, E), timeout: Long, unit: TimeUnit) = add(e)

  override def drainTo(c: util.Collection[_ >: (K, E)], maxElements: Int) = {
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

  override def drainTo(c: util.Collection[_ >: (K, E)]) = drainTo(c, Int.MaxValue)

  override def take() = {
    tickets.take()
    try {
      writeLock.lock()
      flatten(queue.remove())
    } finally {
      retime()
      writeLock.unlock()
    }
  }

  override def remainingCapacity() = Int.MaxValue

  override def peek() = {
    try {
      readLock.lock()
      Option(queue.peek()).map(flatten).orNull
    } finally readLock.unlock()
  }

  override def iterator() = {
    try {
      readLock.lock()
      val iter = queue.iterator()
      new util.Iterator[(K, E)] {
        override def next(): (K, E) = flatten(iter.next())

        override def hasNext: Boolean = iter.hasNext
      }
    } finally readLock.unlock()
  }

  override def size() = {
    try {
      readLock.lock()
      queue.size()
    } finally readLock.unlock()
  }
}