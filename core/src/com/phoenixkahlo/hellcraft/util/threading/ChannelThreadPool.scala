package com.phoenixkahlo.hellcraft.util.threading

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{BlockingQueue, Executors, LinkedBlockingDeque, LinkedBlockingQueue}

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.util.collections.spatial.SpatialTemporalQueue3D
//import com.phoenixkahlo.hellcraft.util.threading.TaskBridger.TaskBuffer

/*
class TaskBridger[P](buffer: TaskBuffer[P], target: Runnable => Unit) {
  val sequence = new FutSequence(target)
  val size = new AtomicInteger(0)

  private def perpetuate(): Unit = {
    val capture = size.getAndUpdate(i => if (i == 0) 0 else i - 1)
    if (capture > 0) {
      buffer.pull().get.run()
      sequence(perpetuate)
    }
  }

  def push(task: Runnable, params: P): Unit = {
    buffer.push(task, params)
    val before = size.getAndIncrement()
    if (before == 0) {
      sequence(perpetuate)
    }
  }
}
object TaskBridger {
  trait TaskBuffer[P] {
    def push(task: Runnable, params: P): Unit
    def pull(): Option[Runnable]
  }
}

class SmartExecutor {
  val queue = new LinkedBlockingDeque[Runnable]

  def execnow(task: Runnable): Unit = queue.addFirst(task)
  def exec(task: Runnable): Unit = ???
  def exec(pos: V3F)(task: Runnable): Unit = ???
  def exec(pos: V2F)(task: Runnable): Unit = ???
  def execc(task: Runnable): Unit = ???
  def execc(pos: V3F)(task: Runnable): Unit = ???
  def execc(pos: V2F)(task: Runnable): Unit = ???
  def point: V3F = ???
  def point_=(p: V3F): Unit = ???
  def close(): Unit = ???
}

/*
class ChannelThreadPool[C <: ChannelThreadPoolConfig](config: C) {
  val pool = Executors.newFixedThreadPool(config.threadCount, config.spawn(_))
  pool.shutdownNow()


}

trait ChannelThreadPoolConfig {
  type Channel <: GenChannel

  def channels: Seq[Channel]
  def spawn(work: Runnable): Thread
  val threadCount: Int
}

object ChannelThreadPoolConfig {
  trait GenChannel {
    def ord: Int
  }
}
*/
*/