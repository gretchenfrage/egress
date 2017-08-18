package com.phoenixkahlo.hellcraft.threading

import java.util
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, ThreadFactory}
import java.util.function.Supplier

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.math.octree.OctreeBlockingQueue

import scala.collection.mutable.ArrayBuffer

class UniExecutor(threadCount: Int, threadFactory: ThreadFactory) {

  private val seqQueue = new LinkedBlockingQueue[Runnable]
  private val octQueue = new OctreeBlockingQueue[Runnable]
  private val quadQueue = new OctreeBlockingQueue[Runnable]
  private val meshQueue = new OctreeBlockingQueue[Runnable]

  private val ticketQueue = new LinkedBlockingQueue[Supplier[Runnable]]
  private val workers = new ArrayBuffer[Thread]

  for (_ <- 1 to threadCount) workers += threadFactory.newThread(() => {
    try {
      while (!Thread.interrupted()) {
        try {
          ticketQueue.take().get().run()
        } catch {
          case e: InterruptedException => throw e
          case e: Exception => println("exception in uniexecutor task (swallowing): " + e)
        }
      }
    } catch {
      case e: InterruptedException => println("uniexecutor thread shutting down")
    }
  })

  def exec(task: Runnable): Unit = {
    seqQueue.add(task)
    ticketQueue.add(() => seqQueue.remove())
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    octQueue.add(pos -> task)
    ticketQueue.add(() => octQueue.remove()._2)
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    quadQueue.add(pos.inflate(0) -> task)
    ticketQueue.add(() => quadQueue.remove()._2)
  }

  def mesh(pos: V3F)(task: Runnable): Unit = {
    meshQueue.add(pos -> task)
    ticketQueue.add(() => meshQueue.remove()._2)
  }

  def point: V3F = octQueue.point
  def point_=(p: V3F): Unit = {
    octQueue.point = p
    quadQueue.point = p.copy(y = 0)
    meshQueue.point = p
  }

  def start(): Unit =
    workers.foreach(_.start())

  def close(): Unit =
    workers.foreach(_.interrupt())

}

object UniExecutor {

  @volatile private var service: UniExecutor = _

  def exec(task: Runnable): Unit = service.exec(task)

  def exec(pos: V3F)(task: Runnable): Unit = service.exec(pos)(task)

  def exec(pos: V2F)(task: Runnable): Unit = service.exec(pos)(task)

  def mesh(pos: V3F)(task: Runnable): Unit = service.mesh(pos)(task)

  def point: V3F = service.point
  def point_=(p: V3F): Unit = service.point = p

  def activate(threadCount: Int, threadFactory: ThreadFactory): Unit = this.synchronized {
    if (service != null) throw new IllegalStateException("uni executor is already active")
    service = new UniExecutor(threadCount, threadFactory)
    service.start()
  }

  def deactivate(): Unit = this.synchronized {
    if (service == null) throw new IllegalStateException("uni executor is not active")
    service.close()
    service = null
  }

}



/*
object UniExecutor {

  @volatile private var service: MultiQueueExecutor = _
  @volatile private var seqQueue: BlockingQueue[Runnable] = _
  @volatile private var octQueue: OctreeBlockingQueue[Runnable] = _
  @volatile private var flatOctQueue: OctreeBlockingQueue[Runnable] = _
  @volatile private var meshQueue: OctreeBlockingQueue[Runnable] = _
  @volatile private var inserterQueue: BlockingQueue[Runnable] = _

  def activate(threadCount: Int, threadFactory: ThreadFactory): Unit = this.synchronized {
    if (service != null) throw new IllegalArgumentException("cannot active uni executor that is already active")

    service = new MultiQueueExecutor(threadCount, threadFactory)

    seqQueue = new LinkedBlockingQueue[Runnable]
    octQueue = new OctreeBlockingQueue[Runnable]
    flatOctQueue = new OctreeBlockingQueue[Runnable]
    meshQueue = new OctreeBlockingQueue[Runnable]
    inserterQueue = new LinkedBlockingQueue[Runnable]

    service.addSource(RunnableSource(seqQueue))
    service.addSource(RunnableSource.fromTupleQueue(octQueue))
    service.addSource(RunnableSource.fromTupleQueue(flatOctQueue))
    service.addSource(RunnableSource.fromTupleQueue(meshQueue))
    service.addSource(RunnableSource(inserterQueue))

    service.start()
  }

  def exec(task: Runnable): Unit = {
    if (service != null)
      seqQueue.add(task)
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    if (service != null)
      inserterQueue.add(() => octQueue.add(pos -> task))
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    if (service != null)
      inserterQueue.add(() => flatOctQueue.add(pos.inflate(0) -> task))
  }

  def mesh(pos: V3F)(task: Runnable): Unit = {
    if (service != null)
      inserterQueue.add(() => meshQueue.add(pos -> task))
  }

  def addQueue(queue: LinkedBlockingQueue[Runnable]): Unit = {
    if (service != null)
      service.addSource(RunnableSource(queue))
  }

  def point: V3F = octQueue.point
  def point_=(p: V3F): Unit = {
    octQueue.point = p
    flatOctQueue.point = p.copy(y = 0)
    meshQueue.point = p
  }

  def deactivate(): Unit = this.synchronized {
    if (service == null) throw new IllegalArgumentException("cannot deactive uni executor that is not active")

    service.stop()

    service = null
    seqQueue = null
    octQueue = null
    flatOctQueue = null
    meshQueue = null
    inserterQueue = null
  }

}
*/