package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.WorldSave
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{HashMap, HashSet, TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, SortedSet, mutable}

/**
  * All methods are thread safe
  */
class ServerContinuum(save: WorldSave) {

  val historySize: Int = 5 * 60

  private val subscriptions = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private val updating = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private var externs = new TreeMap[Long, Set[ChunkEvent]]
  private var history = new TreeMap[Long, ServerWorld]

  history = history.updated(0, new ServerWorld(save, Set.empty, Map.empty, 0))

  def current: ServerWorld = this.synchronized {
    history.last._2
  }

  def time: Long = this.synchronized {
    history.lastKey
  }

  def snapshot(target: Long): Option[ServerWorld] = this.synchronized {
    history.get(target)
  }

  def getStarter(client: ClientID): (Long, Seq[Chunk]) = this.synchronized {
    (time, subscriptions(client).toSeq.map(current.chunkAt(_).get))
  }

  /**
    * Updates the continuum to the next tick, and returns which chunk events need to be routed to which clients.
    */
  def update(): Map[ClientID, SortedSet[ChunkEvent]] = this.synchronized {
    // collect events, including externs, grouped by their producers
    val allSubscribedChunkPositions = subscriptions.values.foldLeft(new HashSet[V3I])(_ ++ _)
    var next = current.setKeepLoaded(allSubscribedChunkPositions)
    val allSubscribedChunks = allSubscribedChunkPositions.toSeq.map(current.chunkAt(_).get)
    val eventsProducedByChunks = allSubscribedChunks.flatMap(chunk => chunk.update(current).map((_, Some(chunk.pos))))
    val externalEvents = externs.get(time).map(_.toSeq.map(event => (event, None))).getOrElse(Nil)
    val events = eventsProducedByChunks ++ externalEvents

    // sort the events and integrate them into a new snapshot, put in history
    next = next.integrate(events.map(_._1).foldLeft(new TreeSet[ChunkEvent])(_ + _)).incrTime
    //history.put(next.time, next)
    history = history.updated(next.time, next)

    // forget history
    if (history.size > historySize)
      history = history.drop(historySize - history.size)

    // sort the events by their targets
    val eventsByTarget: Map[V3I, Seq[(ChunkEvent, Option[V3I])]] =
      events.groupBy(_._1.target)

    // compute which events need to be sent to which clients
    val eventsToSend: Map[ClientID, Seq[ChunkEvent]] = subscriptions.toMap.mapValues({
      subscribed => subscribed.toSeq.map(eventsByTarget.get).filter(_ isDefined).flatMap(_.get)
        .filterNot({ case (_, producer) => producer.exists(subscribed.contains) }).map(_._1)
    })

    // return
    eventsToSend.filter(_._2.nonEmpty).mapValues(_.foldLeft(new TreeSet[ChunkEvent])(_ + _)).filter(_._2.nonEmpty)
  }

  /**
    * Sets the subscriptions for that client at the current tick, and return which chunks need to be sent to that client.
    * While the client could just have the ClientWorld request all these chunks from the server, we send them all at
    * once to improve performance.
    */
  def setClientRelation(client: ClientID, subscribed: Set[V3I], updatingSet: Set[V3I]): Seq[Chunk] = this.synchronized {
    val oldSubscribed: Set[V3I] = subscriptions.getOrElse(client, Set.empty)
    subscriptions.put(client, subscribed)
    updating.put(client, updatingSet)
    (subscribed -- oldSubscribed).toSeq.map(current.chunkAt(_).get)
  }

  /**
    * Revert to that point in time.
    */
  def revert(target: Long): Unit = this.synchronized {
    history = history.rangeImpl(None, Some(target + 1))
  }

  /**
    * Retroactively integrate the events into the continuum, and return similar integration maps that need to be
    * sent to each client.
    */
  def integrateExterns(newExterns: SortedMap[Long, Set[ChunkEvent]]): Map[ClientID, SortedMap[Long, SortedSet[ChunkEvent]]] = this.synchronized {
    // we'll need this later
    val currTime = time

    // discard externs that are impossible to integrate
    val toIntegrate = newExterns.rangeImpl(Some(history.firstKey), None)

    // revert
    revert(toIntegrate.firstKey)

    // introduce the externs
    externs ++= toIntegrate

    // update back to the original time and accumulate the events
    var accumulator = new HashMap[ClientID, SortedMap[Long, SortedSet[ChunkEvent]]]
    while (time < currTime)
      for ((client, events) <- update())
        accumulator = accumulator.updated(client,
          accumulator.getOrElse(client, new TreeMap[Long, SortedSet[ChunkEvent]]).updated(time, events))

    accumulator.filter({ case (_, events) => events.nonEmpty })
  }

}
