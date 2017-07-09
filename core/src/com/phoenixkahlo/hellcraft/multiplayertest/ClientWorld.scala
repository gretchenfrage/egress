package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.SortedSet
import scala.collection.parallel.mutable

class ClientWorld(
                   session: ServerSession,
                   provided: Map[V3I, Chunk],
                   val time: Long
                 ) extends World {

  //TODO: can we improve performance by replacing this with an immutable volatile variable and synchronizing on mutation?
  private val chunks = new mutable.ParHashMap[V3I, Chunk] ++ provided

  override def chunkAt(chunkPos: V3I): Option[Chunk] = {
    chunks.get(chunkPos) match {
      case chunk if chunk isDefined => chunk
      case None =>
        val chunk = session.chunkAt(time, chunkPos).get
        chunks.put(chunkPos, chunk)
        Some(chunk)
    }
  }

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  override def findEntity(id: EventID): Entity =
    chunks.values.flatMap(_.entities).toMap.apply(id)

  def incrTime: ClientWorld =
    new ClientWorld(session, provided, time + 1)

  def integrate(events: SortedSet[ChunkEvent]): ClientWorld = {
    // sort the events by their target
    val eventsByTarget = events.groupBy(_.target)
    // transform the chunks for which events exist
    val transformed: Map[V3I, Chunk] =
      eventsByTarget.par.map({
        case (target, group) => (target, group.foldLeft(chunkAt(target).get)({ case (chunk, event) => event(chunk) }))
      }).seq
    // integrate the transformed chunks into the world
    provide(transformed)
  }

  def provide(chunks: Map[V3I, Chunk]): ClientWorld =
    // combine the existing chunks with the new chunks and construct a new world
    new ClientWorld(session, chunks.toMap.seq ++ chunks, time)

  /**
    * Unload or load chunks as needed.
    */
  def setLoaded(chunks: Set[V3I]): ClientWorld = {
    // TODO: fetch large amounts of chunks from the server more efficiently
    new ClientWorld(session, chunks.map(p => (p, chunkAt(p).get)).toMap, time)
  }

}
