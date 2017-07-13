package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Unit

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
    val avatarOption: Option[Avatar] = server.continuum.snapshot(atTime).map(_.findEntity(avatarID).asInstanceOf[Avatar])
    avatarOption match {
      case Some(avatar) =>
        val event = ChunkEvent(avatar.chunkPos, UUID.randomUUID(),
          _.putEntity(avatar.updateDirection(movDir).updateJumping(jumping)), "set entity movement")
        server.integrateExtern(atTime, event)
      case None => println("setmovement rejected")
    }
  }

}

case class InitialServerData(clientID: ClientID) extends Transmission

case class ServerSessionReady(sessionID: Int) extends Transmission