package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.TreeSet
import scala.collection.{SortedMap, SortedSet, mutable}

class ClientContinuum(session: ServerSession, starter: (Long, Map[V3I, Chunk])) {

  val historySize: Int = 5 * 60

  private var subscribed: Set[V3I] = Set.empty
  private var updating: Set[V3I] = Set.empty
  private var history: mutable.SortedMap[Long, ClientWorld] = new mutable.TreeMap
  starter match {
    case (startTime, startChunks) => history.put(startTime, new ClientWorld(session, startChunks, startTime))
  }

  def current: ClientWorld = this.synchronized {
    history.last._2
  }

  def time: Long = this.synchronized {
    history.last._1
  }

  def revert(target: Long): Unit = this.synchronized {
    history = history.rangeImpl(None, Some(target + 1))
    if (history isEmpty) {
      println("warning: reverted to unrecorded point in history")
      history.put(target, new ClientWorld(session, Map.empty, target).setLoaded(subscribed))
    }
  }

  def update(unpredictable: SortedSet[ChunkEvent] = SortedSet.empty): Unit = this.synchronized {
    /*
    val events: SortedSet[ChunkEvent] = updating.flatMap(current.chunkAt(_).get.update(current))
      .foldLeft(new TreeSet[ChunkEvent])(_ + _) ++ unpredictable

    val newWorld = current.integrate(events).incrTime.setLoaded(subscribed)

    history.put(newWorld.time, newWorld)
    */
    val updated = current.update(subscribed, updating, unpredictable)
    println("previous history = " + history)
    println("putting " + updated.time + " -> " + updated)
    //history.put(updated.time, updated)
    println("FUCK YOU")
    history(updated.time) = updated
    /*
    println("postvious history = " + history)
    println()
    */
  }

  def provide(chunks: Map[V3I, Chunk]): Unit = this.synchronized {
    history.put(time, current.provide(chunks))
  }

  /**
    * This must never revert back into the time-range for which the server has supplied the client with events.
    */
  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk]): Unit = {
    println("clientcontinuum.setserverrelation, provided=" + provided)
    this.synchronized {
      revert(atTime)
      provide(provided)

      subscribed = newSubscribed
      updating = newUpdating
    }
  }

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    this.synchronized {
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

}