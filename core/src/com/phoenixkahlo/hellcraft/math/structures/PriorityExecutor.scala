package com.phoenixkahlo.hellcraft.math.structures

import java.util.concurrent.ThreadFactory

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}

class OctreeExecutor(threads: Int, factory: ThreadFactory) {

  private val queue = new OctreeBlockingQueue[Runnable]

  def point: V3F = queue.point
  def point_=(p: V3F) = queue.point = p

  def execute(pos: V3F)(task: Runnable): Unit = {
    queue.add(pos -> task)
  }

  {
    for (_ <- 1 to threads) {
      factory.newThread(() => {
        while (true) {
          queue.take()._2.run()
        }
      }).start()
    }
  }

}

object OctreeExecutor {

  val global = new OctreeExecutor(
    Runtime.getRuntime.availableProcessors,
    runnable => {
      val thread = new Thread(runnable, "global octree exec thread")
      thread.setPriority(3)
      thread
    }
  )

  def apply(pos: V3F): Runnable => Unit = {
    global.execute(pos)
  }

}

class Octree2DExecutor(threads: Int, factory: ThreadFactory) {

  private val queue = new OctreeBlockingQueue[Runnable]

  def point: V2F = queue.point.flatten
  def point_=(p: V2F) = queue.point = p.inflate(0)

  def execute(pos: V2F)(task: Runnable): Unit = {
    queue.add(pos.inflate(0) -> task)
  }

  {
    for (_ <- 1 to threads) {
      factory.newThread(() => {
        while (true) {
          queue.take()._2.run()
        }
      }).start()
    }
  }

}

object Octree2DExecutor {

  val global = new Octree2DExecutor(
    Runtime.getRuntime.availableProcessors,
    runnable => {
      val thread = new Thread(runnable, "global octree exec thread")
      thread.setPriority(3)
      thread
    }
  )

  def apply(pos: V2F): Runnable => Unit = {
    global.execute(pos)
  }

}