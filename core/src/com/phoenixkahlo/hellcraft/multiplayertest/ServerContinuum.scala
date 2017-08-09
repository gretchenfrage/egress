package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.serial.save.WorldSave
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{HashMap, HashSet, TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, SortedSet, mutable}

/**
  * All methods are thread safe
  */
class ServerContinuum(save: WorldSave) {

  val maxHistorySize: Int = ValidRetroTicks * 8

  private val subscriptions = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private val updating = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private var externs = new TreeMap[Long, Set[ChunkEvent]]
  @volatile private var history =
    new TreeMap[Long, ServerWorld].updated(0, new ServerWorld(save, Set.empty, Map.empty, 0)) // notify on modification

  def current: ServerWorld = this.synchronized {
    history.last._2
  }

  def time: Long = this.synchronized {
    history.lastKey
  }

  def snapshot(target: Long): Option[ServerWorld] = this.synchronized {
    history.get(target)
  }

  def waitForSnapshot(target: Long): Option[ServerWorld] = this.synchronized {
    while (history.lastKey < target)
      wait()
    history.get(target)
  }

  def getStarter(client: ClientID): (Long, Seq[Chunk]) = this.synchronized {
    (time, subscriptions(client).toSeq.map(current.chunkAt(_).get))
  }

  def getSubscribedChunks(client: ClientID, time: Long): Seq[Chunk] = this.synchronized {
    val world = snapshot(time).get
    subscriptions(client).toSeq.map(world.chunkAt(_).get)
  }

  def getHistory: TreeMap[Long, ServerWorld] = this.synchronized {
    history
  }

  def removeClient(client: ClientID): Unit = this.synchronized {
    subscriptions -= client
    updating -= client
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
    history = history.updated(next.time, next)

    // forget history
    if (history.size > maxHistorySize)
      history = history.drop(history.size - maxHistorySize)
    if (externs.size > maxHistorySize)
      externs = externs.drop(externs.size - maxHistorySize)

    // notify
    notifyAll()

    // sort the events by their targets
    val eventsByTarget: Map[V3I, Seq[(ChunkEvent, Option[V3I])]] =
      events.groupBy(_._1.target)

    // compute which events need to be sent to which clients
    val eventsToSend: Map[ClientID, Seq[ChunkEvent]] = subscriptions.toMap.mapValues({
      subscribed =>
        subscribed.toSeq.map(eventsByTarget.get).filter(_ isDefined).flatMap(_.get)
          .filterNot({ case (_, producer) => producer.exists(subscribed.contains) }).map(_._1)
    })

    // return
    val setsToSend: Map[ClientID, SortedSet[ChunkEvent]] =
      eventsToSend.mapValues(_.foldLeft(new TreeSet[ChunkEvent])(_ + _))

    setsToSend
  }

  def setClientRelation(client: ClientID, subscribed: Set[V3I], updatingSet: Set[V3I]): SortedMap[Long, Seq[Chunk]] =
    this.synchronized {
      // find which chunks are newly subscribed to
      val chunks = subscribed -- subscriptions.getOrElse(client, Set.empty)
      // set the relation
      subscriptions.put(client, subscribed)
      updating.put(client, updatingSet)
      // accumulate the chunks to return
      val fromSave = new mutable.HashMap[V3I, Chunk]
      var provide: SortedMap[Long, Seq[Chunk]] = SortedMap.empty
      var chunkSeq = chunks.toSeq
      for (t <- (time - 50) to time) {
        snapshot(t) match {
          case Some(world) =>
            provide = provide.updated(t, chunkSeq.map(p => world.chunkAt(p, {
              fromSave.get(p) match {
                case Some(chunk) => chunk
                case None =>
                  val chunk = save.load(p).get
                  fromSave.put(p, chunk)
                  chunk
              }
            }).get))
          case None =>
        }

      }
      provide
    }

  /**
    * Revert to that point in time.
    */
  def revert(target: Long): Unit = this.synchronized {
    history = history.rangeImpl(None, Some(target + 1))
    notifyAll()
  }

  /**
    * Retroactively integrate the events into the continuum, and return similar integration maps that need to be
    * sent to each client.
    */
  def integrateExterns(newExterns: SortedMap[Long, Set[ChunkEvent]]): Map[ClientID, SortedMap[Long, SortedSet[ChunkEvent]]] =
    this.synchronized {
      // we'll need this later
      val currTime = time

      // discard externs that are impossible to integrate
      val toIntegrate = newExterns.rangeImpl(Some(history.firstKey), None)

      // revert
      revert(toIntegrate.firstKey)

      // introduce the externs
      externs = newExterns.foldLeft(externs)(
        { case (map, (t, set)) => map.updated(t, map.getOrElse(t, Set.empty: Set[ChunkEvent]) ++ set) })

      // update back to the original time and accumulate the events
      var accumulator = new HashMap[ClientID, SortedMap[Long, SortedSet[ChunkEvent]]]
      while (time < currTime)
        for ((client, events) <- update())
          accumulator = accumulator.updated(client,
            accumulator.getOrElse(client, new TreeMap[Long, SortedSet[ChunkEvent]]).updated(time - 1, events))

      accumulator
    }

}
