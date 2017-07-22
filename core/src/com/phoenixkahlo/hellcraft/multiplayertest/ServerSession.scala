package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{AddEntity, Chunk, ChunkEvent, UpdateEntity}
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Boolean

  def avatarCount: Int

  def hashChunk(atTime: Long, p: V3I): Option[Int]

  def getServerNanotime: Long

  def getGameclockStartNanotime: Long

}

class ServerSessionImpl(init: InitialClientData, server: GameServer, clientID: ClientID) extends ServerSession {

  override def getTime: Long = {
    Thread.sleep(rmiLagSimTime)
    server.clock.gametime
  }

  override def chunkAt(time: Long, p: V3I): Option[Chunk] = {
    Thread.sleep(rmiLagSimTime)
    server.continuum.snapshot(time).flatMap(_.chunkAt(p))
  }

  override def getStarter(): (Long, Seq[Chunk]) = {
    Thread.sleep(rmiLagSimTime)
    server.continuum.getStarter(clientID)
  }

  lazy val avatarID: EntityID = {
    Thread.sleep(rmiLagSimTime)
    server.avatars.synchronized {
      while (!server.avatars.contains(clientID))
        server.avatars.wait()
      server.avatars(clientID)
    }
  }

  override def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Boolean = {
    Thread.sleep(rmiLagSimTime)
    server.continuum.snapshot(atTime).flatMap(_.findEntity(avatarID).map(_.asInstanceOf[Avatar])) match {
      case Some(avatar) =>
        server.integrateExtern(atTime,
          UpdateEntity(avatar.updateDirection(movDir).updateJumping(jumping), UUID.randomUUID()))
        true
      case None =>
        false
    }
  }

  override def avatarCount: Int = {
    Thread.sleep(rmiLagSimTime)
    server.continuum.current.loaded.values.flatMap(_.entities.get(avatarID)).size
  }

  override def hashChunk(atTime: Long, p: V3I): Option[Int] = {
    Thread.sleep(rmiLagSimTime)
    server.continuum.snapshot(atTime).flatMap(_.chunkAt(p).map(_.hashCode()))
  }

  override def getServerNanotime: Long = {
    Thread.sleep(rmiLagSimTime)
    System.nanoTime()
  }

  override def getGameclockStartNanotime: Long = {
    Thread.sleep(rmiLagSimTime)
    server.clock.nanotimeStart
  }

}

