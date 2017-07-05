package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I

trait GameSession {

  def getTime: Long

}

class ServerGameSession(client: UUID, server: GameServer) extends GameSession {

  override def getTime: Long = server.world.t


}