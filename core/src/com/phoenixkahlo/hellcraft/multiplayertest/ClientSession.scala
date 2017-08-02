package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.concurrent.{Executors, ThreadLocalRandom}

import com.esotericsoftware.kryonet.rmi.ObjectSpace
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.{SortedMap, SortedSet}

trait ClientSession {

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit

  def setServerRelation(newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Seq[Chunk],
                        unpredictable: SortedMap[Long, SortedSet[ChunkEvent]]): Unit

  def hashChunk(atTime: Long, p: V3I): Option[Int]

  def createSingleThreadSession(threadName: String): Int

}

class ClientSessionImpl(client: EgressClient) extends ClientSession {

  override def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    client.waitForReady()
    client.getContinuum.integrate(events)
  }

  override def setServerRelation(newSubscribed: Set[V3I], newUpdating: Set[V3I],
                                 provided: Seq[Chunk], unpredictable: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    client.waitForReady()
    client.getContinuum.setServerRelation(newSubscribed, newUpdating,
      provided.map(chunk => (chunk.pos, chunk)).toMap, unpredictable)
  }

  override def hashChunk(atTime: Long, p: V3I): Option[Int] = {
    client.getContinuum.snapshot(atTime).flatMap(_.chunkAt(p).map(_.hashCode()))
  }

  override def createSingleThreadSession(threadName: String): Int = {
    val rmiSpace = new ObjectSpace
    rmiSpace.setExecutor(Executors.newSingleThreadExecutor(runnable => new Thread(runnable, threadName)))
    rmiSpace.addConnection(client.getKryonetClient)
    val sessionID = ThreadLocalRandom.current.nextInt()
    // kryonet will break if the same object is registered multiple times for RMI
    rmiSpace.register(sessionID, new ClientSessionImpl(client))
    sessionID
  }
}