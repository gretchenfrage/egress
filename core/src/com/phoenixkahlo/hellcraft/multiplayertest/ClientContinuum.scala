package com.phoenixkahlo.hellcraft.multiplayertest

import java.util
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.Cache

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.{SortedMap, SortedSet, mutable}
import scala.concurrent.duration._

class ClientContinuum(session: ServerSession, getServerTime: => Long) {

  val maxHistorySize = 8 * 60

  private val mutateMutex = this

  @volatile private var subscribed: Set[V3I] = Set.empty
  @volatile private var updating: Set[V3I] = Set.empty
  @volatile private var history: SortedMap[Long, ClientWorld] = {
    val t = getServerTime
    (SortedMap.empty: SortedMap[Long, ClientWorld]).updated(t, new ClientWorld(session, Map.empty, t))
  }
  private val submissionMonitor = new Object
  @volatile private var submissions: SortedMap[Long, SortedSet[ChunkEvent]] = SortedMap.empty

  private val restartThrowback = new AtomicInteger(100)

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

  private sealed trait ServerOperation
  private case class SetRelation(
                                  sub: Set[V3I],
                                  upd: Set[V3I],
                                  prov: Map[V3I, Chunk],
                                  unpredictables: SortedMap[Long, SortedSet[ChunkEvent]]
                                ) extends ServerOperation
  private case class Integrate(
                                events: SortedMap[Long, SortedSet[ChunkEvent]],
                              ) extends ServerOperation {
    def +(o: Integrate): Integrate =
      Integrate(o.events.foldLeft(events)(
        { case (map, (t, set)) => map.updated(t, map.getOrElse(t, SortedSet.empty: SortedSet[ChunkEvent]) ++ set) }))
  }
  private val serverTasks = new LinkedBlockingQueue[ServerOperation]

  private def fetchNewHistory(startT: Long, sub: Set[V3I]): SortedMap[Long, ClientWorld] = {
    println("client continuum: pulling new starter from server at")
    val chunks = session.getChunks(startT, sub)
    (SortedMap.empty: SortedMap[Long, ClientWorld])
      .updated(startT, new ClientWorld(session, chunks.map(c => (c.pos, c)).toMap, startT))
  }

  new Thread(() => {
    while (true) {
      val task = serverTasks.take()
      val startTime = System.nanoTime()
      task match {
        case SetRelation(newSub, newUpd, prov, unpr) =>
          // capture history
          var newHistory = history
          // truncate it
          newHistory = newHistory.rangeImpl(None, Some(unpr.firstKey + 1))
          // provide the chunks
          if (newHistory nonEmpty) {
            newHistory = newHistory.updated(newHistory.lastKey, newHistory.last._2.provide(prov))
          } else {
            newHistory = fetchNewHistory(unpr.firstKey, newSub)
          }
          // define a function to update it
          def upd8() = {
            val t = newHistory.lastKey
            var upd = if (unpr contains t) updating else newUpd
            val newWorld = newHistory.last._2.update(newSub, upd, getSubmitted(t) ++ unpr.getOrElse(t,
              SortedSet.empty: SortedSet[ChunkEvent]))
            newHistory = newHistory.updated(newWorld.time, newWorld)
          }
          // update it as far as submissions will allow
          while (newHistory.lastKey <= submissions.lastOption.map({ case (t, _) => t }).getOrElse(Long.MinValue)) {
            upd8()
          }
          // grab the mutation mutex, catch up completely, and implement the changes
          mutateMutex.synchronized {
            while (newHistory.lastKey <= submissions.lastOption.map({ case (t, _) => t }).getOrElse(Long.MinValue)) {
              upd8()
            }
            subscribed = newSub
            updating = newUpd
            history = newHistory
          }

        case integrate: Integrate =>
          // accumulate more events from the queue if possible
          var task = integrate
          while (serverTasks.size > 0 && serverTasks.element().isInstanceOf[Integrate])
            task += serverTasks.remove().asInstanceOf[Integrate]
          val events: SortedMap[Long, SortedSet[ChunkEvent]] = task.events
          // now to integrate them
          if (events nonEmpty) {
            // capture the history, subscribing, and updating, by first aquiring the mutation mutex
            var (newHistory, sub, upd) = mutateMutex.synchronized {
              (history, subscribed, updating)
            }
            // truncate it
            newHistory = newHistory.rangeImpl(None, Some(events.firstKey + 1))
            if (newHistory isEmpty)
              newHistory = fetchNewHistory(events.firstKey, sub)
            // if it's been truncated such that it no longer contains all the subscribed chunks, fetch a new history
            // TODO: avoid this altogether
            if ({
              val has = newHistory.last._2.getLoadedChunks.keySet
              !subscribed.forall(has.contains)
            }) {
              newHistory = fetchNewHistory(events.firstKey, sub)
            }
            // ensure that all the necessary chunks are present
            if (false) {
              val has = newHistory.last._2.getLoadedChunks.keySet
              if (!subscribed.forall(has.contains))
                System.err.println("insufficient chunks present in integrate procedure")
            }
            // define a function for updating the history with unpredictables
            def upd8(h: SortedMap[Long, ClientWorld], u: SortedSet[ChunkEvent] = SortedSet.empty) = {
              val n = h.last._2.update(sub, upd, u ++ getSubmitted(h.lastKey))
              h.updated(n.time, n)
            }
            // update the history with the accumulated events
            for ((atTime, eventGroup) <- events) {
              while (newHistory.lastKey < atTime)
                newHistory = upd8(newHistory)
              newHistory = upd8(newHistory, eventGroup)
            }
            while (newHistory.lastKey <= submissions.lastKey)
              newHistory = upd8(newHistory)
            // grab the mutation mutex, catch up completely, and implement the changes
            mutateMutex.synchronized {
              while (newHistory.lastKey <= submissions.lastKey)
                newHistory = upd8(newHistory)
              history = newHistory
            }
          }

      }
      val endTime = System.nanoTime()
      val elapsedTime: Duration = ((endTime - startTime) nanoseconds)
      if (elapsedTime > (1 second))
        println("server operating thread stalled!")
    }
  }, "client continuum server operation thread").start()

  def integrate(events: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    val event = Integrate(events)
    serverTasks.add(event)
  }

  def setServerRelation(newSubscribed: Set[V3I], newUpdating: Set[V3I], provided: Map[V3I, Chunk],
                        unpredictable: SortedMap[Long, SortedSet[ChunkEvent]]): Unit = {
    val event = SetRelation(newSubscribed, newUpdating, provided, unpredictable)
    serverTasks.add(event)
  }

}
