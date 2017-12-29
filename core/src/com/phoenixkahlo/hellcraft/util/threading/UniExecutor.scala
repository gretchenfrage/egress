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
  private val dbQueue = new LinkedBlockingQueue[Runnable]
  private val dbQueue3D = new SpatialTemporalQueue3D[Runnable](equator, V3F(1, 2, 1))

  private val ticketQueue = new LinkedBlockingQueue[Supplier[Option[Runnable]]]
  private val workers = new ArrayBuffer[Thread]

  /*
  workers += new Thread(() => {
    try {
      while (true) {
        Thread.sleep(1000)
        println("3D queue size = " + queue3D.size)
        //println("3D queue height = " + octQueue.height)
        println("2D queue size = " + queue2D.size)
        //println("2D queue height = " + quadQueue.height)
        println("seq queue size = " + seqQueue.size)
        println("DB 3D queue size = " + dbQueue3D.size)
        println("DB seq queue size = " + dbQueue.size)
      }
    } catch {
      case e: InterruptedException =>
    }
  })
  */

  def sizeSeq: Int = seqQueue.size
  def size3D: Int = queue3D.size
  def height3D: Int = queue3D.height
  def size2D: Int = queue2D.size
  def height2D: Int = queue2D.height
  def sizeDBSeq: Int = dbQueue.size
  def sizeDB3D: Int = dbQueue3D.size
  def heightDB3D: Int = dbQueue3D.height

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
  workers += threadFactory.newThread(Worker(() => dbQueue.take().run()))
  workers += threadFactory.newThread(Worker(() => dbQueue3D.take()._2.run()))

  def exec(task: Runnable): Unit = {
    seqQueue.add(task)
    ticketQueue.add(() => Option(seqQueue.poll()))
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    exec(() => {
      queue3D.add(pos -> task)
      ticketQueue.add(() => Option(queue3D.poll()).map(_._2))
    })
    //queue3D.add(pos -> task)
    //ticketQueue.add(() => Option(queue3D.poll()).map(_._2))
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    exec(() => {
      queue2D.add(pos -> task)
      ticketQueue.add(() => Option(queue2D.poll()).map(_._2))
    })
    //queue2D.add(pos -> task)
    //ticketQueue.add(() => Option(queue2D.poll()).map(_._2))
  }

  def db(task: Runnable): Unit = {
    dbQueue.add(task)
    ticketQueue.add(() => Option(dbQueue.poll()))
  }

  def db(pos: V3F)(task: Runnable): Unit = {
    if (dbSequential) {
      db(task)
    } else {
      dbQueue3D.add(pos -> task)
      ticketQueue.add(() => Option(dbQueue3D.poll()).map(_._2))
    }
  }

  @volatile private var dbSequential = false

  def makeDBSequential(): Unit = {
    dbSequential = true
    val al = new util.ArrayList[(Any, Runnable)]
    dbQueue3D.drainTo(al)
    val iter = al.iterator()
    while (iter.hasNext) {
      dbQueue.add(iter.next()._2)
    }
  }

  def point: V3F = queue3D.point

  def point_=(p: V3F): Unit = {
    queue3D.point = p
    queue2D.point = p.flatten
    dbQueue3D.point = p
  }

  def start(): Unit =
    workers.foreach(_.start())

  def close(): Unit =
    workers.foreach(_.interrupt())

  def getSpatialTasks: (Seq[V3F], Seq[V2F], Seq[V3F]) = {
    (queue3D.toSeq.map(_._1), queue2D.toSeq.map(_._1), dbQueue3D.toSeq.map(_._1))
  }

}

object UniExecutor {

  @volatile private var service: UniExecutor = _

  def exec(task: Runnable): Unit = service.exec(task)

  def exec(pos: V3F)(task: Runnable): Unit = service.exec(pos)(task)

  def exec(pos: V2F)(task: Runnable): Unit = service.exec(pos)(task)

  def db(task: Runnable): Unit = service.db(task)

  def db(pos: V3F)(task: Runnable): Unit = service.db(pos)(task)

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


