package com.phoenixkahlo.hellcraft.multiplayertest

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
      deltas += -((receiveTime - serverTime) + ((receiveTime - sendTime) / 2))
      Thread.sleep(25)
    }
    val average = deltas.sum / deltas.size
    println("delta = " + average)
    NanotimeMirror(average)
  }

  def mirrorServer(session: ServerSession): NanotimeMirror =
    mirror(session.getServerNanotime)

}