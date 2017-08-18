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

  private val ticketQueue = new LinkedBlockingQueue[Supplier[Option[Runnable]]]
  private val workers = new ArrayBuffer[Thread]

  case class Worker(work: Runnable) extends Runnable {
    override def run(): Unit = {
      try {
        while (!Thread.interrupted()) {
          try {
            work.run()
          } catch {
            case e: InterruptedException => throw e
            case e: Exception => println("exception in uniexecutor task (swallowing): " + e)
          }
        }
      } catch {
        case e: InterruptedException => println("uniexecutor thread shutting down")
      }
    }
  }
  for (_ <- 1 to threadCount)
    workers += threadFactory.newThread(Worker(() => ticketQueue.take().get().foreach(_.run())))
  workers += threadFactory.newThread(Worker(() => seqQueue.take().run()))
  workers += threadFactory.newThread(Worker(() => octQueue.take()._2.run()))
  workers += threadFactory.newThread(Worker(() => quadQueue.take()._2.run()))
  workers += threadFactory.newThread(Worker(() => meshQueue.take()._2.run()))

  def exec(task: Runnable): Unit = {
    seqQueue.add(task)
    ticketQueue.add(() => Option(seqQueue.poll()))
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    octQueue.add(pos -> task)
    ticketQueue.add(() => Option(octQueue.poll()).map(_._2))
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    quadQueue.add(pos.inflate(0) -> task)
    ticketQueue.add(() => Option(quadQueue.poll()).map(_._2))
  }

  def mesh(pos: V3F)(task: Runnable): Unit = {
    meshQueue.add(pos -> task)
    ticketQueue.add(() => Option(meshQueue.poll()).map(_._2))
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


