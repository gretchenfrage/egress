package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.WorldSave
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{HashSet, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}

/**
  * All methods are thread safe
  */
//TODO: don't route events to clients if the event set is actually empty
class ServerContinuum(save: WorldSave) {

  val historySize: Int = 5 * 60

  private val subscriptions = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private val updating = new mutable.HashMap[ClientID, Set[V3I]].withDefaultValue(Set.empty)
  private val externs = new mutable.TreeMap[Long, Set[ChunkEvent]]
  private var history = new mutable.TreeMap[Long, ServerWorld]

  history.put(0, new ServerWorld(save, Set.empty, Map.empty, 0))

  private val currCache = new Cache[ServerWorld](history.last._2)

  def current: ServerWorld = this.synchronized {
    currCache()
  }

  def time: Long = this.synchronized {
    currCache().time
  }

  def snapshot(target: Long): Option[ServerWorld] = {
    this.synchronized {
      history.get(target)
    }

  }

  def getStarter(client: ClientID): (Long, Seq[Chunk]) = this.synchronized {
    (
      time,
      subscriptions(client).toSeq.map(current.chunkAt(_).get)
    )
  }

  /**
    * Updates the continuum to the next tick, and returns which chunk events need to be routed to which clients.
    */
  def update(): Map[ClientID, SortedSet[ChunkEvent]] = this.synchronized {
    // collect events, including externs, grouped by their producers
    val events: Seq[(ChunkEvent, Option[V3I])] =
      subscriptions.values.foldLeft(new HashSet[V3I])(_ ++ _).toSeq.map(current.chunkAt(_).get)
        .flatMap(chunk => chunk.update(current).map((_, Some(chunk.pos)))) ++
        externs.get(time).map(_.toSeq.map(event => (event, None))).getOrElse(Nil)

    // sort the events and integrate them into a new snapshot, put in history, invalidate curr cache
    val next = current.integrate(events.map(_._1).foldLeft(new TreeSet[ChunkEvent])(_ + _)).incrTime
    history.put(next.time, next)
    currCache.invalidate

    // forget history
    if (history.size > historySize)
      history.drop(historySize - history.size)

    // sort the events by their targets
    val eventsByTarget: Map[V3I, Seq[(ChunkEvent, Option[V3I])]] =
      events.groupBy(_._1.target)

    // compute which events need to be sent to which clients
    val eventsToSend: Map[ClientID, Seq[ChunkEvent]] = subscriptions.toMap.mapValues({
      subscribed => subscribed.toSeq.map(eventsByTarget.get).filter(_ isDefined).flatMap(_.get)
        .filterNot({ case (_, producer) => producer.exists(subscribed.contains) }).map(_._1)
    })

    // return
    eventsToSend.mapValues(_.foldLeft(new TreeSet[ChunkEvent])(_ + _))
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
    * Retroactively integrate the events into the continuum, and return simillar integration maps that need to be
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
    val accumulator1 = new mutable.TreeMap[Long, Map[ClientID, SortedSet[ChunkEvent]]]
    while (time < currTime)
      accumulator1.put(time, update())

    // refactor the accumulator
    val accumulator2 = new mutable.TreeMap[ClientID, SortedMap[Long, SortedSet[ChunkEvent]]]
    for (client <- subscriptions.keys) {
      val accumulator3 = new mutable.TreeMap[Long, SortedSet[ChunkEvent]]
      for (time <- accumulator1.keys) {
        accumulator3.put(time, accumulator1(time)(client).foldLeft(new TreeSet[ChunkEvent])(_ + _))
      }
      accumulator2.put(client, accumulator3)
    }

    // return
    accumulator2.toMap
  }

}
