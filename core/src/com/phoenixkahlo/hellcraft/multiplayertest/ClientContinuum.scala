package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}

class ClientContinuum(session: ServerSession, starter: (Long, Map[V3I, Chunk]), getServerTime: => Long) {

  val maxHistorySize = 5 * 60

  private val serverMutex = new Object // synchronize outer
  private val mutateMutex = this // synchronize inner

  @volatile private var subscribed: Set[V3I] = Set.empty // protected by both mutexes
  @volatile private var updating: Set[V3I] = Set.empty // protected by both mutexes
  @volatile private var history: SortedMap[Long, ClientWorld] = starter match { // protected by only mutate mutex
    case (startTime, startChunks) => new TreeMap[Long, ClientWorld]()
      .updated(startTime, new ClientWorld(session, startChunks, startTime))
  }

  def time: Long = history.lastKey

  def current: ClientWorld = history.last._2

  def snapshot(atTime: Long): Option[ClientWorld] = history.get(atTime)

  def update(unpredictable: SortedSet[ChunkEvent] = SortedSet.empty): Unit = mutateMutex.synchronized {
    val updated = current.update(subscribed, updating, unpredictable)
    history = history.updated(updated.time, updated)
  }

  /**
    * A server operation
    */
  def provide(chunks: Map[V3I, Chunk]): Unit = serverMutex.synchronized {
    mutateMutex.synchronized {
      history = history.updated(time, current.provide(chunks))
    }
  }

  /**
    * A server operation
    */
  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk]
                       ): Unit = serverMutex.synchronized {
    // capture the history
    var newHistory = history
    // truncate it (revert)
    newHistory = newHistory.rangeImpl(None, Some(atTime + 1))
    // provide the chunks TODO: think about if this could be causing a problem
    newHistory = newHistory.updated(newHistory.lastKey, newHistory.last._2.provide(provided))
    // update it back to the current time
    while (newHistory.lastKey < getServerTime) {
      val newWorld = newHistory.last._2.update(newSubscribed, newUpdating, SortedSet.empty)
      newHistory = newHistory.updated(newWorld.time, newWorld)
    }
    // grab the mutation mutex and implement the changes
    mutateMutex.synchronized {
      subscribed = newSubscribed
      updating = newUpdating
      history = newHistory
    }
  }

  /**
    * A server operation
    */
  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    if (events isEmpty) return
    serverMutex.synchronized {
      // capture the history
      var newHistory = history
      // truncate it (revert)
      newHistory = newHistory.rangeImpl(None, Some(events.firstKey + 1))
      // define a function for updating the history with unpredictables
      def upd8(h: SortedMap[Long, ClientWorld], u: SortedSet[ChunkEvent] = SortedSet.empty) = {
        val n = h.last._2.update(subscribed, updating, u)
        h.updated(n.time, n)
      }
      // update it with the events
      for ((atTime, eventGroup) <- events) {
        while (newHistory.lastKey < atTime)
          newHistory = upd8(newHistory)
        newHistory = upd8(newHistory, eventGroup)
      }
      while (newHistory.lastKey < getServerTime)
        newHistory = upd8(newHistory)
      // grab the mutation mutex and implement the changes
      mutateMutex.synchronized {
        history = newHistory
      }
    }
  }

}
