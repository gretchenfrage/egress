package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.{SortedMap, SortedSet}

trait ClientSession {

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit

  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Seq[Chunk]): Unit

}

class ClientSessionImpl(init: InitialServerData, client: GameClient) extends ClientSession {

  override def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    println("client: integrating " + events)
    client.waitForReady()
    client.getContinuum.integrate(events)
  }

  override def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I],
                                 provided: Seq[Chunk]): Unit = {
    client.waitForReady()
    client.getContinuum.setServerRelation(atTime, newSubscribed, newUpdating,
      provided.map(chunk => (chunk.pos, chunk)).toMap)
  }

}

case class InitialClientData() extends Transmission
case class ClientSessionReady(sessionID: Int) extends Transmission