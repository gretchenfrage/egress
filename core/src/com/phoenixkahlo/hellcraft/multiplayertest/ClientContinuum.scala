package com.phoenixkahlo.hellcraft.multiplayertest

import java.util
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}

//TODO: forget
class ClientContinuum(session: ServerSession, getServerTime: => Long) {

  val maxHistorySize = 5 * 60

  private val mutateMutex = this

  @volatile private var subscribed: Set[V3I] = Set.empty
  @volatile private var updating: Set[V3I] = Set.empty
  @volatile private var history: SortedMap[Long, ClientWorld] = {
    val t = getServerTime
    (SortedMap.empty: SortedMap[Long, ClientWorld]).updated(t, new ClientWorld(session, Map.empty, t))
  }
  private val submissionMonitor = new Object
  @volatile private var submissions: SortedMap[Long, SortedSet[ChunkEvent]] = SortedMap.empty

  private val restartThrowback = new AtomicInteger(20)

  def time: Long = history.lastKey

  def current: ClientWorld = history.last._2

  def snapshot(atTime: Long): Option[ClientWorld] = history.get(atTime)

  def update(submit: SortedSet[ChunkEvent]): Unit = mutateMutex.synchronized {
    // submit the events
    submissionMonitor.synchronized {
      submissions = submissions.updated(time, submit)
      submissionMonitor.notifyAll()
    }
    // prune history
    submissions = submissions.rangeImpl(Some(time - maxHistorySize), None)
    history = history.rangeImpl(Some(time - maxHistorySize), None)
    // update
    if (time < getServerTime) {
      val updated = current.update(subscribed, updating, submit)
      history = history.updated(updated.time, updated)
    }
  }

  private def getSubmitted(atTime: Long): SortedSet[ChunkEvent] = {
    submissions.get(atTime) match {
      case Some(events) => events
      case None => submissionMonitor.synchronized {
        while (submissions.isEmpty || submissions.lastKey < atTime)
          submissionMonitor.wait()
        submissions.getOrElse(atTime, SortedSet.empty)
      }
    }
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

  private def fetchNewHistory(startT: Long): SortedMap[Long, ClientWorld] = {
    println("client continuum: pulling new starter from server at")
    val chunks = session.getSubscribedChunks(startT)
    (SortedMap.empty: SortedMap[Long, ClientWorld])
      .updated(startT, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, startT))
  }

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
            newHistory = fetchNewHistory(t)
          }
          // update it back to the current time minus one (because of submissions)
          while (newHistory.lastKey < getServerTime && submissions.contains(newHistory.lastKey)) {
            val newWorld = newHistory.last._2.update(sub, upd, getSubmitted(newHistory.lastKey))
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
          val events: SortedMap[Long, SortedSet[ChunkEvent]] = task.events
          // now to integrate them
          if (events nonEmpty) {
            // capture the history
            var newHistory = history
            // truncate it
            newHistory = newHistory.rangeImpl(None, Some(events.firstKey + 1))
            if (newHistory isEmpty) {
              //newHistory = fetchNewHistory(getServerTime - restartThrowback.getAndUpdate(_ * 2))
              newHistory = fetchNewHistory(events.firstKey)
            }
            // define a function for updating the history with unpredictables
            def upd8(h: SortedMap[Long, ClientWorld], u: SortedSet[ChunkEvent] = SortedSet.empty) = {
              val n = h.last._2.update(subscribed, updating, u ++ getSubmitted(h.lastKey))
              h.updated(n.time, n)
            }
            // update the history with the accumulated events
            for ((atTime, eventGroup) <- events) {
              while (newHistory.lastKey < atTime)
                newHistory = upd8(newHistory)
              newHistory = upd8(newHistory, eventGroup)
            }
            while (newHistory.lastKey < getServerTime && submissions.contains(newHistory.lastKey))
              newHistory = upd8(newHistory)
            // grab the mutation mutex and implement the changes
            mutateMutex.synchronized {
              history = newHistory
            }
          }
          // notify all that we've finished
          for (n <- task.not) {
            n.synchronized {
              n.notify()
            }
          }
      }
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

}
