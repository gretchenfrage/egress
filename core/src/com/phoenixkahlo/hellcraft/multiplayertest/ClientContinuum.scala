package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}

class ClientContinuum(session: ServerSession, starter: (Long, Map[V3I, Chunk]), getServerTime: => Long) {

  val maxHistorySize = 5 * 60

  //private val serverMutex = new Object // synchronize outer
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

  sealed trait ServerOperation
  case class SetRelation(atTime: Long, sub: Set[V3I], upd: Set[V3I], prov: Map[V3I, Chunk], not: Object) extends ServerOperation
  case class Integrate(events: SortedMap[Long, SortedSet[ChunkEvent]], not: Seq[Object]) extends ServerOperation {
    def +(o: Integrate): Integrate =
      Integrate(
        o.events.foldLeft(events)({ case (map, (t, set)) => map.updated(t, map.getOrElse(t, SortedSet.empty: SortedSet[ChunkEvent]) ++ set) }),
        o.not ++ not
      )
  }
  private val serverTasks = new LinkedBlockingQueue[ServerOperation]

  new Thread(() => {
    while (true) {
      serverTasks.take() match {
        case SetRelation(t, sub, upd, prov, not) =>
          // capture history
          var newHistory = history
          // truncate it
          newHistory = newHistory.rangeImpl(None, Some(t + 1))
          // provide the chunks
          if (newHistory nonEmpty) {
            newHistory = newHistory.updated(newHistory.lastKey, newHistory.last._2.provide(prov))
          } else {
            println("client continuum: pulling new starter from server")
            val startT = getServerTime - 20
            val chunks = session.getSubscribedChunks(startT)
            newHistory = new TreeMap[Long, ClientWorld]()
              .updated(startT, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, startT))
          }
          // update it back to the current time
          while (newHistory.lastKey < getServerTime) {
            val newWorld = newHistory.last._2.update(sub, upd, SortedSet.empty)
            newHistory = newHistory.updated(newWorld.time, newWorld)
          }
          // grab the mutation mutex and implement the changes
          mutateMutex.synchronized {
            subscribed = sub
            updating = upd
            history = newHistory
          }
          // notify that we've finished
          not.synchronized {
            not.notify()
          }

        case integrate: Integrate =>
          // accumulate more events from the queue if possible
          var task = integrate
          while (serverTasks.size > 0 && serverTasks.element().isInstanceOf[Integrate])
            task += serverTasks.remove().asInstanceOf[Integrate]
          val events = task.events
          // now to integrate them
          if (events nonEmpty) {
            // capture the history
            var newHistory = history
            // truncate it
            newHistory = newHistory.rangeImpl(None, Some(events.firstKey + 1))
            if (newHistory isEmpty) {
              println("client continuum: pulling new starter from server")
              val startT = getServerTime - 20
              val chunks = session.getSubscribedChunks(startT)
              newHistory = new TreeMap[Long, ClientWorld]()
                .updated(startT, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, startT))
            }
            // define a function for updating the history with unpredictables
            def upd8(h: SortedMap[Long, ClientWorld], u: SortedSet[ChunkEvent] = SortedSet.empty) = {
              val n = h.last._2.update(subscribed, updating, u)
              h.updated(n.time, n)
            }
            // update the history with the accumulated events
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
          // notify all that we've finished
          //task.not.foreach(_.notify())
          for (n <- task.not) {
            n.synchronized {
              n.notify()
            }
          }
      }
      println("client continuum: task completed")
    }
  }, "client continuum server operation thread").start()

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    val event = Integrate(events, Seq(new Object))
    serverTasks.add(event)
    event.not.head.synchronized {
      event.not.head.wait()
    }
  }

  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk]
                       ): Unit = {
    val event = SetRelation(atTime, newSubscribed, newUpdating, provided, new Object)
    serverTasks.add(event)
    event.not.synchronized {
      event.not.wait()
    }
  }

  /**
    * A server operation
    */
  /*
  def setServerRelation(atTime: Long, newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk]
                       ): Unit = serverMutex.synchronized {
    // capture the history
    var newHistory = history
    // truncate it (revert)
    newHistory = newHistory.rangeImpl(None, Some(atTime + 1))
    // provide the chunks
    if (newHistory nonEmpty)
      newHistory = newHistory.updated(newHistory.lastKey, newHistory.last._2.provide(provided))
    else {
      println("client continuum: pulling new starter from the server")
      val t = getServerTime - 20
      val chunks = session.getSubscribedChunks(t)
      newHistory = new TreeMap[Long, ClientWorld]()
        .updated(t, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, t))
    }
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
      if (newHistory isEmpty) {
        println("client continuum: pulling new starter from server")
        val t = getServerTime - 20
        val chunks = session.getSubscribedChunks(t)
        newHistory = new TreeMap[Long, ClientWorld]()
          .updated(t, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, t))
      }
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

  def integrate(event: ChunkEvent, atTime: Long): Unit =
    integrate(new TreeMap[Long, SortedSet[ChunkEvent]]().updated(atTime, new TreeSet[ChunkEvent] + event))
  */

}
