package com.phoenixkahlo.hellcraft.util.debugging

import java.io.PrintStream

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class Profiler(name: String) {

  val times = new ArrayBuffer[Long]
  log()

  def log(): Unit =
    times += System.nanoTime()

  def deltas: Seq[Duration] = {
    val buffer = new ArrayBuffer[Duration]
    for (i <- 1 until times.size) {
      buffer += ((times(i) - times(i - 1)) nanoseconds)
    }
    buffer.to[Vector]
  }

  def print(out: PrintStream = System.out): Unit = {
    out.print(name + ": [")
    for (d <- deltas) {
      out.print(d.toMillis + "ms, ")
    }
    out.println("]")
  }

  def printDisc(threshhold: Double, out: PrintStream = System.out): Boolean = {
    if (deltas.find(_ > (threshhold milliseconds)).isDefined) {
      print(out)
      true
    } else false
  }

}

object Profiler {

  def apply(name: String): Profiler =
    new Profiler(name)

}