package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

}

class ServerSessionImpl(client: UUID, server: GameServer) extends ServerSession {

  override def getTime: Long = server.continuum.time

  override def chunkAt(time: Long, p: V3I): Option[Chunk] =
    server.continuum.snapshot(time).flatMap(_.chunkAt(p))

}