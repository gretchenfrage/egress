package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I

import scala.collection.SortedSet
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.{ParHashMap, ParMap}

class ClientWorld(
                 session: ServerSession,
                 provided: Map[V3I, Chunk],
                 val time: Long
                 ) extends World {

  private val monitor = new Object
  @volatile private var chunks: Map[V3I, Chunk] = provided

  override def chunkAt(p: V3I): Option[Chunk] = {
    chunks.get(p) match {
      case chunk if chunk isDefined => chunk
      case None =>
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

  override def findEntity(id: EntityID): Entity = chunks.values.flatMap(_.entities.get(id)).head

  /**
    * Take care to also integrate events, including unpredictable events pulled from the server, as this function
    * literally just creates an exact copy with incremented time.
    * Also, make sure to first unload chunks which are not subscribed to.
    */
  def incrTime: ClientWorld =
    new ClientWorld(session, provided, time + 1)

  def provide(newChunks: Map[V3I, Chunk]): ClientWorld =
    new ClientWorld(session, chunks ++ newChunks, time)

  def setLoaded(ps: Set[V3I]): ClientWorld = {
    new ClientWorld(session, ps.map(p => (p, chunkAt(p).get)).toMap, time)
  }

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
/*
class ClientWorld(
                   session: ServerSession,
                   provided: Map[V3I, Chunk],
                   val time: Long
                 ) extends World {

  //TODO: can we improve performance by replacing this with an immutable volatile variable and synchronizing on mutation?
  //TODO: would that also fix the other weird glitch
  private val chunks: mutable.ParMap[V3I, Chunk] = new mutable.ParHashMap[V3I, Chunk] ++ provided

  override def chunkAt(chunkPos: V3I): Option[Chunk] = {
    chunks.get(chunkPos) match {
      case chunk if chunk isDefined => chunk
      case None =>
        val chunk = session.chunkAt(time, chunkPos).get
        chunks.put(chunkPos, chunk)
        Some(chunk)
    }
  }

  def weakChunkAt(p: V3I): Option[Chunk] =
    chunks.get(p)

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
  def setLoaded(ps: Set[V3I]): ClientWorld = {
    //println("clientworld loading new chunks: " + (ps -- chunks.keys))
    // TODO: fetch large amounts of chunks from the server more efficiently
    new ClientWorld(session, ps.map(p => (p, chunkAt(p).get)).toMap, time)
  }

}
*/