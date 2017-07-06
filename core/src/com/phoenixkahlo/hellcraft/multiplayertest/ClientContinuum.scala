package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.TreeSet
import scala.collection.{SortedMap, SortedSet, mutable}

class ClientContinuum(startTime: Long) {

  val historySize: Int = 5 * 60

  private var subscribed: Set[V3I] = Set.empty
  private var updating: Set[V3I] = Set.empty
  private var history = new mutable.TreeMap[Long, ClientWorld]

  private val currCache = new Cache[ClientWorld](history.last._2)

  def current: ClientWorld = this.synchronized {
    currCache()
  }

  def time: Long = this.synchronized {
    currCache().time
  }

  def revert(target: Long): Unit = {
    history = history.rangeImpl(None, Some(target + 1))
  }

  def update(unpredictable: SortedSet[ChunkEvent] = SortedSet.empty): Unit = this.synchronized {
    val events: SortedSet[ChunkEvent] = updating.flatMap(current.chunkAt(_).get.update(current, time))
      .foldLeft(new TreeSet[ChunkEvent])(_ + _) ++ unpredictable

    val newWorld = current.integrate(events).incrTime.setLoaded(subscribed)

    history.put(newWorld.time, newWorld)
  }

  def provide(chunks: Map[V3I, Chunk]): Unit = this.synchronized {
    history.put(time, current.provide(chunks))
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
