package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, PutEntity}
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Unit

  def avatarCount: Int

  def hashChunk(atTime: Long, p: V3I): Option[Int]

}

class ServerSessionImpl(init: InitialClientData, server: GameServer, clientID: ClientID) extends ServerSession {

  override def getTime: Long = server.continuum.time

  override def chunkAt(time: Long, p: V3I): Option[Chunk] = {
    server.continuum.snapshot(time).flatMap(_.chunkAt(p))
  }

  override def getStarter(): (Long, Seq[Chunk]) =
    server.continuum.getStarter(clientID)

  lazy val avatarID: EntityID = server.avatars.synchronized {
    while (!server.avatars.contains(clientID))
      server.avatars.wait()
    server.avatars(clientID)
  }

  override def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Unit = {
    server.continuum.snapshot(atTime).flatMap(_.findEntity(avatarID).map(_.asInstanceOf[Avatar])) match {
      case Some(avatar) =>
        val event = PutEntity(avatar.chunkPos, avatar.updateDirection(movDir).updateJumping(jumping), UUID.randomUUID())
        println("setting avatar movement")
        server.integrateExtern(atTime, event)
      case None =>
        println("setmovement rejected - failed to find avatar")
    }
  }

  override def avatarCount: Int =
    server.continuum.current.loaded.values.flatMap(_.entities.get(avatarID)).size

  override def hashChunk(atTime: Long, p: V3I): Option[Int] =
    server.continuum.snapshot(atTime).flatMap(_.chunkAt(p).map(_.hashCode()))

}

case class InitialServerData(clientID: ClientID) extends Transmission

case class ServerSessionReady(sessionID: Int) extends Transmission