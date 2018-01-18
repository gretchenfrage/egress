package com.phoenixkahlo.hellcraft.util.collections.queue

import java.util.concurrent.{ConcurrentLinkedDeque, ConcurrentLinkedQueue}

import com.phoenixkahlo.hellcraft.math.{Origin, Origin2D, V2F, V3F}
import com.phoenixkahlo.hellcraft.util.collections.spatial.{Octree, Quadtree, SpatialPriorityQueue}
import com.phoenixkahlo.hellcraft.util.collections.spatial.Octree.Octree

class PoolLinearQueue[T](emptyListener: Boolean => Unit) extends Queue[T] {
  private val internal = new java.util.LinkedList[T]

  override def out: QueueOut[T] = () => PoolLinearQueue.this.synchronized {
    try internal.poll()
    finally emptyListener(!internal.isEmpty)
  }

  override def in: QueueIn[T] = t => PoolLinearQueue.this.synchronized {
    try internal.add(t)
    finally emptyListener(!internal.isEmpty)
  }
}

class PoolOctreeQueue[T](emptyListener: Boolean => Unit) extends Queue[(V3F, T)] {
  private val internal = new SpatialPriorityQueue[V3F, T](Octree.bigEmpty, Origin)

  def point: V3F = this.synchronized {
    internal.point
  }

  def point_=(p: V3F): Unit = this.synchronized {
    internal.point = p
  }

  override def out: QueueOut[(V3F, T)] = () => PoolOctreeQueue.this.synchronized {
    try internal.poll()
    finally emptyListener(!internal.isEmpty)
  }

  override def in: QueueIn[(V3F, T)] = t => PoolOctreeQueue.this.synchronized {
    try internal.add(t)
    finally emptyListener(!internal.isEmpty)
  }
}

class PoolQuadtreeQueue[T](emptyListener: Boolean => Unit) extends Queue[(V2F, T)] {
  private val internal = new SpatialPriorityQueue[V2F, T](Quadtree.bigEmpty, Origin2D)

  def point: V2F = this.synchronized {
    internal.point
  }

  def point_=(p: V2F): Unit = this.synchronized {
    internal.point = p
  }

  override def out: QueueOut[(V2F, T)] = () => PoolQuadtreeQueue.this.synchronized {
    try internal.poll()
    finally emptyListener(!internal.isEmpty)
  }

  override def in: QueueIn[(V2F, T)] = t => PoolQuadtreeQueue.this.synchronized {
    try internal.add(t)
    finally emptyListener(!internal.isEmpty)
  }
}