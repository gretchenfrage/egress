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
  case class Config(threadCount: Int, onException: Exception => Unit, stretch: V2F, bgPriority: Int, fgPriority: Int)

  sealed trait PoolStatus
  case object Unstarted extends PoolStatus
  case object Running extends PoolStatus
  case object Closed extends PoolStatus
}

class SmartPool(config: SmartPool.Config) extends UniExecutor {
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
  private val main3D = new PoolOctreeQueue[Runnable](setStatus(2), V3F(config.stretch.x, config.stretch.y, config.stretch.x))
  private val main2D = new PoolQuadtreeQueue[Runnable](setStatus(3), Ones2D)
  private val critSeq = new PoolLinearQueue[Runnable](setStatus(4))
  private val crit3D = new PoolOctreeQueue[Runnable](setStatus(5), V3F(config.stretch.x, config.stretch.y, config.stretch.x))
  private val crit2D = new PoolQuadtreeQueue[Runnable](setStatus(6), Ones2D)

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
    object CloseException extends Exception
    try while (!Thread.interrupted()) {
      {
        var task: Runnable = null
        var priorityHigh = false
        while ({ task = foreground.out.poll(); task != null }) {
          if (!priorityHigh) {
            Thread.currentThread().setPriority(config.fgPriority)
            priorityHigh = true
          }
          runTask(task)
        }
        if (priorityHigh)
          Thread.currentThread().setPriority(config.bgPriority)
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
              if (poolStatus == Closed)
                throw CloseException
              if (Thread.interrupted())
                throw new InterruptedException
              while (poolStatus == Running && status.get() == 0)
                monitor.wait()
            }
          }
          notif.decrementAndGet()
        }
      }
    } catch {
      case CloseException =>
        println("pool thread closing")
      case e: InterruptedException =>
        println("pool thread interrupted")
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

  override def point: V3F = main3D.point

  override def point_=(p: V3F): Unit = {
    main3D.point = p
    crit3D.point = p
    main2D.point = p.flatten
    crit2D.point = p.flatten
  }

  def start(): Unit = {
    if (_poolStatus.getAndUpdate({
      case Unstarted => Running
      case other => other
    }) != Unstarted)
      throw new IllegalStateException("Starting pool requires it to be in unstarted state")
    else for (i <- 1 to config.threadCount) {
      val thread = new Thread(procedure)
      thread.setPriority(config.bgPriority)
      thread.setName("smart pool worker")
      workers += thread
      thread.start()
    }
  }

  override def simplifyCrits(): Unit = {
    crit3D.out.map(_._2).drainTo(critSeq.in)
    crit2D.out.map(_._2).drainTo(critSeq.in)
    afterAdd()
  }

  def close(exec: Runnable => Unit = _.run()): Promise = {
    // atomically get and update pool status, then compare and branch.
    // if we're running, set it to closing, and resume
    // otherwise, throw an expception
    // being in closing status will cause workers to terminate when they're out of tasks
    if (_poolStatus.getAndUpdate({
      case Running => Closed
      case other => other
    }) != Running)
      throw new IllegalStateException("Closing pool requires it to be in running state")
    else Promise({
      // interrupt all threads
      monitor.synchronized {
        for (thread <- workers)
          thread.interrupt()
      }
      // join all threads
      for (thread <- workers)
        thread.join()
    }, exec)
  }

}

object SmartPoolTest extends App {
  import scala.concurrent.duration._

  for (threadCount <- 1 to 32) {
    val config = SmartPool.Config(threadCount, _.printStackTrace(), Ones2D, 10, 10)
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