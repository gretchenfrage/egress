package com.phoenixkahlo.hellcraft.util.collections.spatial

import java.util
import java.util.concurrent.TimeUnit

import com.phoenixkahlo.hellcraft.math.{V3F, V4F}

import scala.concurrent.duration.Duration

class SpatialTemporalPriorityQueue[E](timePerMeter: Duration) extends java.util.AbstractQueue[(V3F, E)] with java.util.concurrent.BlockingQueue[(V3F, E)] {
  private val nanosPerMeter: Long = timePerMeter.toNanos
  private val beginNanos: Long = System.nanoTime()
  private def addTime(v: V3F): V4F = v.inflate((System.nanoTime() - beginNanos).toFloat / nanosPerMeter)

  private val blocking = new SpatialBlockingQueue[V4F, E](HexadecaTree.empty(V4F(0, 0, 0, 0), Float.MaxValue), V4F(0, 0, 0, Float.MinValue))
  private val minTime = new java.util.PriorityQueue[Float]()

  private def removeTime(w: Float): Unit = {
    minTime.remove(w)
    val p = blocking.point
    blocking.point = V4F(p.x, p.y, p.z, minTime.peek())
  }

  def point: V3F = blocking.point.flatten

  def point_=(v: V3F): Unit = {
    blocking.point = v.inflate(
      if (minTime.isEmpty) Float.MinValue
      else minTime.peek()
    )
  }

  override def drainTo(c: util.Collection[_ >: (V3F, E)], maxElements: Int): Int =
    throw new UnsupportedOperationException

  override def drainTo(c: util.Collection[_ >: (V3F, E)]): Int =
    throw new UnsupportedOperationException

  override def take(): (V3F, E) = {
    val (k, v) = blocking.take()
    removeTime(k.w)
    (k.flatten, v)
  }

  override def put(e: (V3F, E)): Unit = {
    val (k, v) = e
    val kt = addTime(k)
    blocking.put(kt -> v)
    minTime.add(kt.w)
  }

  override def remainingCapacity(): Int = {
    blocking.remainingCapacity()
  }

  override def offer(e: (V3F, E)): Boolean = {
    val (k, v) = e
    val kt = addTime(k)
    if (blocking.offer(kt -> v)) {
      minTime.add(kt.w)
      true
    } else false
  }

  override def offer(e: (V3F, E), timeout: Long, unit: TimeUnit): Boolean = offer(e)

  override def poll(): (V3F, E) = {
    val r = blocking.poll()
    if (r != null) {
      val (k, v) = r
      removeTime(k.w)
      k.flatten -> v
    } else null
  }

  override def poll(timeout: Long, unit: TimeUnit): (V3F, E) = poll()

  override def size(): Int = blocking.size()

  override def iterator(): util.Iterator[(V3F, E)] = {
    val i = blocking.iterator()
    new util.Iterator[(V3F, E)] {
      override def next(): (V3F, E) = {
        val (k, v) = i.next()
        k.flatten -> v
      }

      override def hasNext: Boolean = i.hasNext
    }
  }

  override def peek(): (V3F, E) = {
    Option(blocking.peek()).map({ case (k, v) => k.flatten -> v }).orNull
  }
}
