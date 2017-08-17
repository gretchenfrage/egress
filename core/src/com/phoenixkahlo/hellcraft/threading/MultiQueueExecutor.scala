package com.phoenixkahlo.hellcraft.threading

import java.util.concurrent._

import com.sun.jmx.remote.internal.ArrayQueue

import scala.collection.mutable.ArrayBuffer

class MultiQueueExecutor(threadCount: Int, threadFactory: ThreadFactory) {

  if (threadCount < 1)
    throw new IllegalArgumentException("executor must have at least one thread")

  @volatile private var started = false
  @volatile private var stopped = false
  private val toAdd = new LinkedBlockingQueue[RunnableSource]
  @volatile private var activeQueues = Vector[RunnableSource]()
  @volatile private var helperThreads = new ArrayBuffer[Thread]()
  private var managerThread = new Thread(() => {
    while (true) {
      // if the active pool is empty
      if (activeQueues.isEmpty) {
        // kill all the helper threads
        helperThreads.foreach(_.interrupt())
        // block until toAdd contains a queue
        activeQueues +:= toAdd.take()
        // add more queues if present
        while (toAdd.size() > 0)
          activeQueues +:= toAdd.remove()
        // respawn the helpers
        spawnHelpers()
      }
      // poll the queues, removing them from the active pool if necessary
      for (worker <- activeQueues) {
        worker.poll() match {
          case null =>
            // if a queue fails to produce a task, make a thread to wait on it's awakening
            activeQueues = activeQueues.filterNot(_ == worker)
            new Thread(() => {
              val task = worker.take()
              task.run()
              toAdd.add(worker)
            }, "waiter thread").start()
          case task => task.run()
        }
      }
    }
    // move new or reawakened queues into the active pool
    while (toAdd.size > 0)
      activeQueues +:= toAdd.remove()
  }, "manager thread")
  private def spawnHelpers(): Unit = {
    helperThreads = new ArrayBuffer[Thread]
    for (_ <- 1 until threadCount) {
      helperThreads += new Thread(() => {
        while (!Thread.interrupted()) {
          // take a task from the most bloated queue and runnest
          val activeCapture = activeQueues
          if (activeCapture nonEmpty)
            Option(activeCapture(ThreadLocalRandom.current.nextInt(activeCapture.size)).poll()).foreach(_.run())
        }
      }, "helper thread")
    }
    helperThreads.foreach(_.start())
  }

  def start(): Unit = this.synchronized {
    if (started) throw new IllegalStateException("cannot start executor that is already started")
    managerThread.start()
    started = true
  }

  def addSource(source: RunnableSource): Unit = {
    toAdd.add(source)
  }

  def stop(): Unit = this.synchronized {
    if (stopped) throw new IllegalStateException("cannot stop executor that is already stopped")
    if (!started) throw new IllegalStateException("cannot stop executor that isn't started")
    managerThread.interrupt()
    managerThread.join()
    for (helper <- helperThreads)
      helper.interrupt()
    stopped = true
  }

}

case class RunnableSource(poll: () => Runnable, take: () => Runnable, size: () => Int)

object RunnableSource {

  def apply(queue: BlockingQueue[Runnable]): RunnableSource =
    RunnableSource(() => queue.poll(), () => queue.take(), () => queue.size)

  def fromTupleQueue[E](queue: BlockingQueue[(E, Runnable)]): RunnableSource =
    RunnableSource(
      () => queue.poll() match {
        case null => null
        case (_, task) => task
      },
      () => queue.take()._2,
      () => queue.size
    )

}

object MultiQueueExecTest extends App {

  val exec = new MultiQueueExecutor(8, new Thread(_, "multi executor thread"))
  exec.start()
  val queue = new LinkedBlockingQueue[Runnable]
  exec.addSource(RunnableSource(queue))

  while (true) {
    if (queue.size() < 1000000)
      queue.add(() => {
        val r = Math.abs(ThreadLocalRandom.current().nextInt(1000000))
        val p = Stream.iterate(2)(_ * 2).filter(_ >= r).head
        println("first power of 2 that's at least " + r + " is " +p)
      })
  }

}