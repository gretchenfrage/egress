package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.atomic.AtomicInteger

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.util.collections.queue._

class SmartPool(threadCount: Int, threadFac: Runnable => Thread) {
  private val index = new AtomicInteger(0)
  private val status = new AtomicInteger(0x0)
  private val notif = new AtomicInteger(0)
  private val monitor = new Object

  private def setStatus(i: Int)(b: Boolean): Unit =
    if (b) status.updateAndGet(_ | (0x1 << i))
    else status.updateAndGet(_ & ~(0x1 << i))

  private val foreground = new PoolLinearQueue[Runnable](setStatus(0))

  private val mainSeq = new PoolLinearQueue[Runnable](setStatus(1))
  private val main3D = new PoolOctreeQueue[Runnable](setStatus(2))
  private val main2D = new PoolQuadtreeQueue[Runnable](setStatus(3))
  private val critSeq = new PoolLinearQueue[Runnable](setStatus(4))
  private val crit3D = new PoolOctreeQueue[Runnable](setStatus(5))
  private val crit2D = new PoolQuadtreeQueue[Runnable](setStatus(6))

  private val out = Array[QueueOut[Runnable]](
    mainSeq.out,
    main3D.out.map(_._2),
    main2D.out.map(_._2),
    critSeq.out,
    crit3D.out.map(_._2),
    crit2D.out.map(_._2)
  )

  private val procedure: Runnable = () => {
    while (true) {
      {
        var task: Runnable = null
        while ({ task = foreground.out.poll(); task != null })
          task.run()
      }

      val i = index.getAndUpdate(i => (i + 1) % out.length)
      val polled = out(i).poll()
      if (polled != null) {
        polled.run()
      } else {
        if (status.get() == 0) {
          notif.incrementAndGet()
          if (status.get() == 0 ) {
            monitor.synchronized {
              while (status.get() == 0)
                monitor.wait()
            }
          }
          notif.decrementAndGet()
        }
      }
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

  for (i <- 1 to threadCount)
    threadFac(procedure).start()

}
