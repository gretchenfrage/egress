package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.ThreadFactory

import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V2F, V3F}
import com.thesamet.spatial.{KDTree, KDTreeMap}

class SpatialExecutor(threads: Int, threadFactory: ThreadFactory) {

  @volatile private var tasks = KDTreeMap.empty[V3F, Runnable]
  private val taskMonitor = new Object

  @volatile var priorityPoint: V3F = Origin

  private def takeTask(): Runnable = {
    taskMonitor.synchronized {
      while (tasks isEmpty)
        taskMonitor.wait()
      val (pos, task) = tasks.findNearest(priorityPoint, 1).head
      tasks -= pos
      task
    }
  }

  def execute(pos: V3F)(task: Runnable): Unit = {
    taskMonitor.synchronized {
      var insertPos = pos
      var delta = 1f
      while (tasks contains insertPos) {
        insertPos += Repeated(delta)
        delta /= 2
      }
      tasks += insertPos -> task
      taskMonitor.notifyAll()
    }
  }

  {
    val executionRoutine: Runnable = () => {
      while (true) {
        takeTask().run()
      }
    }
    for (_ <- 1 to threads) {
      threadFactory.newThread(executionRoutine).start()
    }
  }

  override def toString: String = {
    "SpatialExecutor(" + tasks + ")"
  }

}

object SpatialExecutor {

  implicit val global = new SpatialExecutor(Runtime.getRuntime.availableProcessors, runnable => {
    val thread = new Thread(runnable, "global spatial exec thread")
    thread.setPriority(3)
    thread
  })

}

class SpatialExecutor2D(threads: Int, threadFactory: ThreadFactory) {

  @volatile private var tasks = KDTreeMap.empty[V2F, Runnable]
  private val taskMonitor = new Object

  @volatile var priorityPoint: V2F = V2F(0, 0)

  private def takeTask(): Runnable = {
    taskMonitor.synchronized {
      while (tasks isEmpty)
        taskMonitor.wait()
      val (pos, task) = tasks.findNearest(priorityPoint, 1).head
      tasks -= pos
      task
    }
  }

  def execute(pos: V2F)(task: Runnable): Unit = {
    taskMonitor.synchronized {
      var insertPos = pos
      var delta = 1f
      while (tasks contains insertPos) {
        insertPos += V2F(delta, delta)
        delta /= 2
      }
      tasks += insertPos -> task
      taskMonitor.notifyAll()
    }
  }

  {
    val executionRoutine: Runnable = () => {
      while (true) {
        takeTask().run()
      }
    }
    for (_ <- 1 to threads) {
      threadFactory.newThread(executionRoutine).start()
    }
  }

  override def toString: String = {
    "SpatialExecutor(" + tasks + ")"
  }

}

object SpatialExecutor2D {

  implicit val global = new SpatialExecutor2D(Runtime.getRuntime.availableProcessors, runnable => {
    val thread = new Thread(runnable, "global spatial exec 2D thread")
    thread.setPriority(3)
    thread
  })

}
