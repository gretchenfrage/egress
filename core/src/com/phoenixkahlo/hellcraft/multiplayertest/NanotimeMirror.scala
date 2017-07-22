package com.phoenixkahlo.hellcraft.multiplayertest

import com.esotericsoftware.kryonet.Server

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer

case class NanotimeMirror(delta: Long) {

  def nanotime: Long = System.nanoTime() + delta

}

object NanotimeMirror {

  def mirror(queryRemoteTime: => Long): NanotimeMirror = {
    var deltas = new ArrayBuffer[Long]
    for (_ <- 1 to 10) {
      val sendTime = System.nanoTime()
      val serverTime = queryRemoteTime
      val receiveTime = System.nanoTime()
      println("round trip time = " + (receiveTime - sendTime) / 1000000 + " ms")
      deltas += (receiveTime - serverTime) + ((receiveTime - sendTime) / 2)
      Thread.sleep(25)
    }
    val average = deltas.sum / deltas.size
    println("deltas = " + deltas + ", average = " + average / 1000000 + " ms")
    NanotimeMirror(average)
  }

  def mirrorServer(session: ServerSession): NanotimeMirror =
    mirror(session.getServerNanotime)

}