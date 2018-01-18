package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.phoenixkahlo.hellcraft.math.{Ones2D, V2F, V3F}
import com.phoenixkahlo.hellcraft.util.collections.queue._
import com.phoenixkahlo.hellcraft.util.debugging.Profiler
import com.phoenixkahlo.hellcraft.util.threading.SmartPool.{Closed, PoolStatus, Running, Unstarted}
import com.phoenixkahlo.hellcraft.util.time.Timer

import scala.collection.mutable


object SmartPool {
  case class Config(threadCount: Int, threadFac: Runnable => Thread, onException: Exception => Unit, stretch: V2F)

  sealed trait PoolStatus
  case object Unstarted extends PoolStatus
  case object Running extends PoolStatus
  case object Closed extends PoolStatus
}

class SmartPool(config: SmartPool.Config) {
  if (config.threadCount < 1)
    throw new IllegalArgumentException("Pool must have at least 1 thread")
  if (config.stretch.x <= 0 || config.stretch.y <= 0)
    throw new IllegalArgumentException("Pool stretch components must be positive")

  private val _poolStatus = new AtomicReference[PoolStatus](Unstarted)
  def poolStatus = _poolStatus.get()

  private val index = new AtomicInteger(0)
  private val status = new AtomicInteger(0x0)
  private val notif = new AtomicInteger(0)
  private val monitor = new Object

  private def setStatus(i: Int)(b: Boolean): Unit =
    if (b) status.updateAndGet(_ | (0x1 << i))
    else status.updateAndGet(_ & ~(0x1 << i))

  private val foreground = new PoolLinearQueue[Runnable](setStatus(0))

  private val mainSeq = new PoolLinearQueue[Runnable](setStatus(1))
  private val main3D = new PoolOctreeQueue[Runnable](setStatus(2), config.stretch.inflate(1))
  private val main2D = new PoolQuadtreeQueue[Runnable](setStatus(3), config.stretch)
  private val critSeq = new PoolLinearQueue[Runnable](setStatus(4))
  private val crit3D = new PoolOctreeQueue[Runnable](setStatus(5), config.stretch.inflate(1))
  private val crit2D = new PoolQuadtreeQueue[Runnable](setStatus(6), config.stretch)

  private val out = Array[QueueOut[Runnable]](
    mainSeq.out,
    main3D.out.map(_._2),
    main2D.out.map(_._2),
    critSeq.out,
    crit3D.out.map(_._2),
    crit2D.out.map(_._2)
  )

  private val workers = new mutable.ArrayBuffer[Thread]

  private def runTask(task: Runnable): Unit = {
    try {
      task.run()
    } catch {
      case e: Exception => config.onException(e)
    }
  }

  private val procedure: Runnable = () => {
    try while (!Thread.interrupted()) {
      {
        var task: Runnable = null
        while ({ task = foreground.out.poll(); task != null })
          runTask(task)
      }

      val i = index.getAndUpdate(i => (i + 1) % out.length)
      val polled = out(i).poll()
      if (polled != null) {
        runTask(polled)
      } else {
        if (status.get() == 0) {
          notif.incrementAndGet()
          if (status.get() == 0 ) {
            monitor.synchronized {
              if (Thread.interrupted())
                throw new InterruptedException
              while (status.get() == 0)
                monitor.wait()
            }
          }
          notif.decrementAndGet()
        }
      }
    } catch {
      case e: InterruptedException => //println("smart pool shutting down")
    }
  }

  private def afterAdd(): Unit = {
    if (notif.get() > 0) monitor.synchronized {
      monitor.notifyAll()
    }
  }

  def exec(task: Runnable): Unit = {
    mainSeq.in.push(task)
    afterAdd()
  }

  def exec(pos: V3F)(task: Runnable): Unit = {
    main3D.in.push(pos -> task)
    afterAdd()
  }

  def exec(pos: V2F)(task: Runnable): Unit = {
    main2D.in.push(pos -> task)
    afterAdd()
  }

  def execc(task: Runnable): Unit = {
    critSeq.in.push(task)
    afterAdd()
  }

  def execc(pos: V3F)(task: Runnable): Unit = {
    crit3D.in.push(pos -> task)
    afterAdd()
  }

  def execc(pos: V2F)(task: Runnable): Unit = {
    crit2D.in.push(pos -> task)
    afterAdd()
  }

  def foreground(task: Runnable): Unit = {
    foreground.in.push(task)
    afterAdd()
  }

  def start(): Unit = {
    if (_poolStatus.getAndUpdate({
      case Unstarted => Running
      case other => other
    }) != Unstarted)
      throw new IllegalStateException("Starting pool requires it to be in unstarted state")
    else for (i <- 1 to config.threadCount) {
      val thread = config.threadFac(procedure)
      workers += thread
      thread.start()
    }
  }


  def close(exec: Runnable => Unit = _.run()): Promise = {
    if (_poolStatus.getAndUpdate({
      case Running => Closed
      case other => other
    }) != Running)
      throw new IllegalStateException("Closing pool requires it to be in running state")
    else Promise({
      monitor.synchronized {
        for (thread <- workers)
          thread.interrupt()
      }
      for (thread <- workers)
        thread.join()

    }, exec)
  }
}

object SmartPoolTest extends App {
  import scala.concurrent.duration._

  for (threadCount <- 1 to 32) {
    val config = SmartPool.Config(threadCount, procedure => {
      val thread = new Thread(procedure)
      thread.setPriority(10)
      thread
    }, _.printStackTrace(), Ones2D)
    val pool = new SmartPool(config)
    pool.start()

    val profiler = Profiler(threadCount + " threads")

    val nums: Seq[Fut[Int]] =
      for (i <- 1 to 100) yield Fut({
        // spin lock wait for 10 ms to simulate actual CPU work
        val timer = Timer.start
        while (timer.elapsed < (10 milliseconds)) ()
        10
      }, pool.exec)

    val sum: Fut[Int] = nums.reduce((f1, f2) => f1.flatMap(n1 => f2.map(n2 => n1 + n2, pool.exec)))

    sum.await

    profiler.log()
    profiler.print()

    pool.close()
  }
}