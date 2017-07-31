package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.SortedSet

class ClientWorld(
                 session: ServerSession,
                 provided: Map[V3I, Chunk],
                 val time: Long,
                 providedEntityPosHint: Map[EntityID, V3I] = Map.empty
                 ) extends World {

  private val monitor = new Object
  @volatile private var chunks: Map[V3I, Chunk] = provided
  @volatile private var entityPosHints: Map[EntityID, V3I] = providedEntityPosHint

  override def chunkAt(p: V3I): Option[Chunk] = {
    chunks.get(p) match {
      case chunk if chunk isDefined => chunk
      case None =>
        println("client fetching chunk from server at " + p)
        val chunk = session.chunkAt(time, p).get
        monitor.synchronized { chunks = chunks.updated(p, chunk) }
        Some(chunk)
    }
  }

  def weakChunkAt(p: V3I): Option[Chunk] =
    chunks.get(p)

  def getLoadedChunks: Map[V3I, Chunk] =
    chunks

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  //override def findEntity(id: EntityID): Option[Entity] = chunks.values.flatMap(_.entities.get(id)).headOption
  override def findEntity(id: EntityID): Option[Entity] = {
    entityPosHints.get(id).flatMap(chunkAt(_).get.entities.get(id)) match {
      case entity if entity isDefined => entity
      case None =>
        chunks.values.flatMap(chunk => chunk.entities.get(id).map(entity => (chunk.pos, entity))).headOption match {
          case Some((p, entity)) =>
            entityPosHints = entityPosHints.updated(id, p)
            Some(entity)
          case None => None
        }
    }
  }

  /**
    * Take care to also integrate events, including unpredictable events pulled from the server, as this function
    * literally just creates an exact copy with incremented time.
    * Also, make sure to first unload chunks which are not subscribed to.
    */
  def incrTime: ClientWorld =
    new ClientWorld(session, provided, time + 1, entityPosHints)

  def provide(newChunks: Map[V3I, Chunk]): ClientWorld =
    new ClientWorld(session, chunks ++ newChunks, time, entityPosHints)

  def setLoaded(ps: Set[V3I]): ClientWorld = {
    new ClientWorld(session, ps.toSeq.map(p => (p, chunkAt(p).get)).toMap, time, entityPosHints)
  }

  def integrate(events: SortedSet[ChunkEvent]): ClientWorld = {
    // sort the events by their target
    val eventsByTarget: Map[V3I, SortedSet[ChunkEvent]] = events.groupBy(_.target)
    // transform the chunks for which events exist
    val transformed: Map[V3I, Chunk] =
    //TODO: parallelize this
      eventsByTarget.map({
        case (target, group) => (target, group.foldLeft(chunkAt(target).get)({ case (chunk, event) => event(chunk) }))
      })
    // integrate the transformed chunks into the world
    provide(transformed)
  }

  def update(subscribed: Set[V3I], updating: Set[V3I], unpredictable: SortedSet[ChunkEvent]): ClientWorld = {
    // accumulate all the events from chunks we are updating, and then join them with the unpredictable events
    // filter out events targeted at chunks we are not subscribed to
    val events = updating.flatMap(chunkAt(_).get.update(this)).filter(e => subscribed.contains(e.target))
      .to[SortedSet] ++ unpredictable
    // unload chunks that we will not receive the events for
    // then integrate the events into our chunks, which should require 0 chunk-fetches from server
    // and finally, increment the time
    this.setLoaded(subscribed).integrate(events).incrTime
  }

}