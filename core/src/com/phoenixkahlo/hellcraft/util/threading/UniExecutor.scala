package com.phoenixkahlo.hellcraft.util.threading

import java.util
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, ThreadFactory}
import java.util.function.{Consumer, Supplier}

import com.phoenixkahlo.hellcraft.math.{V2F, V2I, V3F}
import com.phoenixkahlo.hellcraft.util.collections.spatial._

import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer

class UniExecutor(threadCount: Int, threadFactory: ThreadFactory, failHandler: Consumer[Throwable], equator: Long => Float) {

  private val seqQueue = new LinkedBlockingQueue[Runnable]
  private val queue3D = new SpatialTemporalQueue3D[Runnable](equator, V3F(1, 2, 1))
  private val queue2D = new SpatialTemporalQueue2D[Runnable](equator, V2I(1, 1))
  private val cQueue = new LinkedBlockingQueue[Runnable]
  private val cQueue3D = new SpatialTemporalQueue3D[Runnable](equator, V3F(1, 2, 1))

  private val ticketQueue = new LinkedBlockingQueue[Supplier[Option[Runnable]]]
  private val workers = new ArrayBuffer[Thread]

  def sizeSeq: Int = seqQueue.size
  def size3D: Int = queue3D.size
  def height3D: Int = queue3D.height
  def size2D: Int = queue2D.size
  def height2D: Int = queue2D.height
  def sizeCSeq: Int = cQueue.size
  def sizeC3D: Int = cQueue3D.size
  def heightC3D: Int = cQueue3D.height

  case class Worker(work: Runnable) extends Runnable {
    override def run(): Unit = {
      try {
        while (!Thread.interrupted()) {
          try {
            work.run()
          } catch {
            case e: InterruptedException => throw e
            case e: Exception => failHandler.accept(e)
          }
        }
      } catch {
        case e: InterruptedException => //println("uniexecutor thread shutting down")
      }
    }
  }

  for (_ <- 1 to threadCount)
    workers += threadFactory.newThread(Worker(() => ticketQueue.take().get().foreach(_.run())))
  workers += threadFactory.newThread(Worker(() => seqQueue.take().run()))
  workers += threadFactory.newThread(Worker(() => queue3D.take()._2.run()))
  workers += threadFactory.newThread(Worker(() => queue2D.take()._2.run()))
  workers += threadFactory.newThread(Worker(() => cQueue.take().run()))
  workers += threadFactory.newThread(Worker(() => cQueue3D.take()._2.run()))

  def exec(task: Runnable): Unit = {
    seqQueue.add(task)
    ticketQueue.add(() => Option(seqQueue.poll()))
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    exec(() => {
      queue3D.add(pos -> task)
      ticketQueue.add(() => Option(queue3D.poll()).map(_._2))
    })
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    exec(() => {
      queue2D.add(pos -> task)
      ticketQueue.add(() => Option(queue2D.poll()).map(_._2))
    })
  }

  def execc(task: Runnable): Unit = {
    cQueue.add(task)
    ticketQueue.add(() => Option(cQueue.poll()))
  }

  def execc(pos: V3F)(task: Runnable): Unit = {
    if (dbSequential) {
      execc(task)
    } else {
      cQueue3D.add(pos -> task)
      ticketQueue.add(() => Option(cQueue3D.poll()).map(_._2))
    }
  }

  @volatile private var dbSequential = false

  def makeCSequential(): Unit = {
    dbSequential = true
    val al = new util.ArrayList[(Any, Runnable)]
    cQueue3D.drainTo(al)
    val iter = al.iterator()
    while (iter.hasNext) {
      cQueue.add(iter.next()._2)
    }
  }

  def point: V3F = queue3D.point

  def point_=(p: V3F): Unit = {
    queue3D.point = p
    queue2D.point = p.flatten
    cQueue3D.point = p
  }

  def start(): Unit =
    workers.foreach(_.start())

  def close(): Unit =
    workers.foreach(_.interrupt())

  def getSpatialTasks: (Seq[V3F], Seq[V2F], Seq[V3F]) = {
    (queue3D.toSeq.map(_._1), queue2D.toSeq.map(_._1), cQueue3D.toSeq.map(_._1))
  }

}

object UniExecutor {

  @volatile private var service: UniExecutor = _

  def exec(task: Runnable): Unit = service.exec(task)

  def exec(pos: V3F)(task: Runnable): Unit = service.exec(pos)(task)

  def exec(pos: V2F)(task: Runnable): Unit = service.exec(pos)(task)

  def execc(task: Runnable): Unit = service.execc(task)

  def execc(pos: V3F)(task: Runnable): Unit = service.execc(pos)(task)

  def point: V3F = service.point

  def point_=(p: V3F): Unit = service.point = p

  def getService: UniExecutor = service

  def activate(threadCount: Int, threadFactory: ThreadFactory, failHandler: Consumer[Throwable], equator: Long => Float): Unit =
    this.synchronized {
      if (service != null) throw new IllegalStateException("uni executor is already active")
      service = new UniExecutor(threadCount, threadFactory, failHandler, equator)
      service.start()
    }

  def deactivate(): Unit = this.synchronized {
    if (service == null) throw new IllegalStateException("uni executor is not active")
    service.close()
    service = null
  }

}


