package com.phoenixkahlo.hellcraft.util

import java.util.concurrent.{BlockingQueue, LinkedBlockingDeque, TimeoutException}

import com.phoenixkahlo.hellcraft.math.{Origin, Repeated, V3F, V3I}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Awaitable, CanAwait}
import scala.concurrent.duration.Duration

trait MeshCompiler {

  def await: (Array[Float], Array[Short])

  def isCompleted: Boolean

}

case class InstantMeshCompiler(compiler: () => (Array[Float], Array[Short])) extends MeshCompiler {

  val compiled = compiler()

  override def await: (Array[Float], Array[Short]) = compiled

  override def isCompleted: Boolean = true

}

case class BackgroundMeshCompiler(location: V3F, compiler: () => (Array[Float], Array[Short]))
  extends MeshCompiler {

  private val queue: BlockingQueue[(Array[Float], Array[Short])] = new LinkedBlockingDeque

  override def await: (Array[Float], Array[Short]) =
    queue.peek()

  override def isCompleted: Boolean =
    queue.size() > 0

  def onCompleted(compiled: (Array[Float], Array[Short])): Unit =
    queue.add(compiled)

  def dist(v: V3F): Float =
    location dist v

  def compile(): (Array[Float], Array[Short]) =
    compiler()

  BackgroundMeshCompilerExecutor.enqueue(this)

}

object BackgroundMeshCompilerExecutor {

  private val pullMonitor = new Object
  private val pullFrom = new ArrayBuffer[BackgroundMeshCompiler]
  @volatile private var playerPos: V3F = Origin

  def setPlayerPos(pos: V3F): Unit =
    playerPos = pos

  def enqueue(compiler: BackgroundMeshCompiler): Unit =
    pullMonitor.synchronized {
      pullFrom += compiler
      pullMonitor.notifyAll()
    }

  // create the worker threads
  for (_ <- 1 to Runtime.getRuntime.availableProcessors) {
    val thread = new Thread(() => {
      while (true) {

        var compiler: BackgroundMeshCompiler = null
        while (compiler == null) {
          pullMonitor.synchronized {
            while (pullFrom isEmpty)
              pullMonitor.wait()
            val v = playerPos
            pullFrom.sortBy(_ dist v).headOption match {
              case Some(c) =>
                compiler = c
                pullFrom.remove(pullFrom.indexOf(c))
              case None => println("mesh compiler thread awakened but there isn't a compiler")
            }
          }
        }

        val compiled = compiler.compile()
        compiler.onCompleted(compiled)

      }
    })
    thread.setPriority(2)
    thread.start()
  }


}