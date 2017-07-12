package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Unit

}

class ServerSessionImpl(init: InitialClientData, server: GameServer, client: UUID) extends ServerSession {

  override def getTime: Long = server.continuum.time

  override def chunkAt(time: Long, p: V3I): Option[Chunk] = {
    server.continuum.snapshot(time).flatMap(_.chunkAt(p))
  }

  override def getStarter(): (Long, Seq[Chunk]) =
    server.continuum.getStarter(client)

  override def avatarID: EntityID =

  override def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Unit = ???

}

case class InitialServerData(clientID: ClientID) extends Transmission
case class ServerSessionReady(sessionID: Int) extends Transmission