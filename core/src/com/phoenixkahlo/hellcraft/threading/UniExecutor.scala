package com.phoenixkahlo.hellcraft.threading

import java.util
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, ThreadFactory}

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.math.structures.OctreeBlockingQueue

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
    if (inserterQueue != null)
      inserterQueue.add(() => meshQueue.add(pos -> task))
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