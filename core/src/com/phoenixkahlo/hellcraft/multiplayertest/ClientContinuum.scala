package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}

class ClientContinuum(session: ServerSession, starter: (Long, Map[V3I, Chunk])) {

  val historySize = 5 * 60

  @volatile private var subscribed: Set[V3I] = Set.empty
  @volatile private var updating: Set[V3I] = Set.empty
  @volatile private var history: SortedMap[Long, ClientWorld] = starter match {
    case (startTime, startChunks) => new TreeMap[Long, ClientWorld]()
      .updated(startTime, new ClientWorld(session, startChunks, startTime))
  }

  def time: Long = history.lastKey

  def current: ClientWorld = history.last._2

  def snapshot(atTime: Long): Option[ClientWorld] = history.get(atTime)

  def revert(target: Long): Unit = this.synchronized {
    history = history.rangeImpl(None, Some(target + 1))
    if (history isEmpty) {
      println("warning: reverted past recorded history")
      history = history.updated(target, new ClientWorld(session, Map.empty, target).setLoaded(subscribed))
    }
  }

  def update(unpredictable: SortedSet[ChunkEvent] = SortedSet.empty): Unit = this.synchronized {
    val updated = current.update(subscribed, updating, unpredictable)
    history = history.updated(updated.time, updated)
  }

  def provide(chunks: Map[V3I, Chunk]): Unit = this.synchronized {
    history = history.updated(time, current.provide(chunks))
  }

  /**
    * This must never revert back into the time-range for which the server has supplied the client with events.
    */
  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk]): Unit =
    this.synchronized {
      val currTime = time

      revert(atTime)
      provide(provided)
      subscribed = newSubscribed
      updating = newUpdating

      while (time < currTime)
        update()
    }

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = this.synchronized {
    if (events isEmpty) return

    val currTime = time
    revert(events.firstKey)

    for ((targetTime, eventGroup) <- events) {
      while (time < targetTime)
        update()
      update(eventGroup)
    }

    while (time < currTime)
      update()
  }

}
