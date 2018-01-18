package com.phoenixkahlo.hellcraft.util.collections.queue

import java.util.concurrent.{ConcurrentLinkedDeque, ConcurrentLinkedQueue}

import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.spatial.{Octree, Quadtree, SpatialPriorityQueue}
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree.Octree

class PoolLinearQueue[T](emptyListener: Boolean => Unit) extends Queue[T] {
  private val internal = new java.util.LinkedList[T]

  override def out: QueueOut[T] = new QueueOut[T] {
    override def poll(): T = PoolLinearQueue.this.synchronized {
      try internal.poll()
      finally emptyListener(!internal.isEmpty)
    }

    override def drainTo(consumer: (T) => Unit): Unit = PoolLinearQueue.this.synchronized {
      while (!internal.isEmpty)
        consumer(internal.remove())
      emptyListener(false)
    }
  }

  override def in: QueueIn[T] = t => PoolLinearQueue.this.synchronized {
    try internal.add(t)
    finally emptyListener(!internal.isEmpty)
  }
}

class PoolOctreeQueue[T](emptyListener: Boolean => Unit, stretch: V3F = Ones) extends Queue[(V3F, T)] {
  private val internal = new SpatialPriorityQueue[V3F, T](Octree.bigEmpty, Origin)

  def point: V3F = this.synchronized {
    internal.point \\ stretch
  }

  def point_=(p: V3F): Unit = this.synchronized {
    internal.point = p ** stretch
  }

  override def out: QueueOut[(V3F, T)] = new QueueOut[(V3F, T)] {
    override def poll(): (V3F, T) = PoolOctreeQueue.this.synchronized {
      try internal.poll()
      finally emptyListener(!internal.isEmpty)
    }

    override def drainTo(consumer: ((V3F, T)) => Unit): Unit = PoolOctreeQueue.this.synchronized {
      internal.drainTo(consumer)
      emptyListener(false)
    }
  }

  override def in: QueueIn[(V3F, T)] = t => PoolOctreeQueue.this.synchronized {
    try internal.add((t._1 ** stretch, t._2))
    finally emptyListener(!internal.isEmpty)
  }
}

class PoolQuadtreeQueue[T](emptyListener: Boolean => Unit, stretch: V2F = Ones2D) extends Queue[(V2F, T)] {
  private val internal = new SpatialPriorityQueue[V2F, T](Quadtree.bigEmpty, Origin2D)

  def point: V2F = this.synchronized {
    internal.point \\ stretch
  }

  def point_=(p: V2F): Unit = this.synchronized {
    internal.point = p ** stretch
  }

  override def out: QueueOut[(V2F, T)] = new QueueOut[(V2F, T)] {
    override def poll(): (V2F, T) = PoolQuadtreeQueue.this.synchronized {
      try internal.poll()
      finally emptyListener(!internal.isEmpty)
    }

    override def drainTo(consumer: ((V2F, T)) => Unit): Unit = PoolQuadtreeQueue.this.synchronized {
      internal.drainTo(consumer)
      emptyListener(false)
    }
  }

  override def in: QueueIn[(V2F, T)] = t => PoolQuadtreeQueue.this.synchronized {
    try internal.add((t._1 ** stretch, t._2))
    finally emptyListener(!internal.isEmpty)
  }
}