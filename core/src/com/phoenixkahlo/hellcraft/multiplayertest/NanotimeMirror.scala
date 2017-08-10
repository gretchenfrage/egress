package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.concurrent.ThreadLocalRandom

import com.esotericsoftware.kryonet.Server

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer

case class NanotimeMirror(delta: Long) {

  def nanotime: Long = System.nanoTime() + delta

}

object NanotimeMirror {

  def mirror(queryRemoteTime: => Long): NanotimeMirror = {
    val deltas = new ArrayBuffer[Long]
    for (_ <- 1 to 10) {
      val sendTime = System.nanoTime()
      val serverTime = queryRemoteTime
      val receiveTime = System.nanoTime()
      deltas += ((serverTime - sendTime) + (serverTime - receiveTime)) / 2
      Thread.sleep(25)
    }
    val average = deltas.sum / deltas.size
    println("delta = " + average / 100000 + " ms")
    NanotimeMirror(average)
  }

  def mirrorServer(session: ServerSession): NanotimeMirror =
    mirror(session.getServerNanotime)

}

object MirrorTest {

  def main(args: Array[String]): Unit = {

    /*
    val delta = ThreadLocalRandom.current.nextInt()
    println("real delta = " + delta)

    val latency = 200 // ms
    def query: Long = {
      Thread.sleep(latency)
      val t = System.nanoTime() + delta
      Thread.sleep(latency)
      t
    }

    val mirror = NanotimeMirror.mirror(query)

    while (true) {
      val t1 = System.nanoTime() + delta
      val t2 = mirror.nanotime
      println("times = [" + t1 + ", " + t2 + "], diff = " + (t1 - t2) / 1000000 + " ms")
      Thread.sleep(1000)
    }
    */
  }

}