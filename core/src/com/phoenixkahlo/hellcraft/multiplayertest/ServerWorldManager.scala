package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.{Block, Chunk, ChunkEvent}
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, Entity}
import com.phoenixkahlo.hellcraft.gamedriver.GameDriver
import com.phoenixkahlo.hellcraft.infinitetest.HashCacheWorld
import com.phoenixkahlo.hellcraft.math.{Directions, V3F, V3I}
import com.phoenixkahlo.hellcraft.save.WorldSave

import scala.collection.immutable.{HashSet, IndexedSeq, TreeSet}
import scala.collection.{Map, SortedMap, SortedSet, mutable}
import scala.concurrent.duration._

/**
  * Purely functional, server-side snapshot of the world and involved data, which wraps a HashCacheWorld, as well as
  * some other relevant data.
  *
  * The main operations on this are:
  *
  * settings whichs chunks a particular client subscribes to
  * updating the world into the next frame, providing the necessary externs
  *
  * These operations are purely functional, and create a new ServerWorld, but they also produce data representing
  * side-effects - to the save file and the networked clients - that should be handled by the caller.
  *
  * Also, these operations may pull data from a save buffer. It is assumed that the buffer will not be modified during
  * the time these operations are called, and as such, the buffer and the wrapped save directory can be viewed as if
  * they were a immutable data collections (which is infinite, lazily evalutated, and stored on the hard drive).
  *
  * All chunks that any client is subscribing to must be loaded in the world.
  */
case class ServerWorld(
                   world: HashCacheWorld,
                   subscriptions: Map[ClientID, Set[V3I]]
                 ) {

  def updateSubscribed(client: ClientID, chunks: Set[V3I], puller: V3I => Chunk): SWStateMonad = {
    val newSubscriptions = subscriptions.updated(client, chunks)
    val toLoad = newSubscriptions(client) -- subscriptions.values.flatten
    val toUnload = subscriptions.values.flatten.toSet -- newSubscriptions.values.flatten.toSet
    SWStateMonad(
      ServerWorld(
        world ++ toLoad.map(puller).toSeq,
        newSubscriptions
      ),
      BufferPush(toUnload.map(world.chunkAt(_).get).toSeq)
    )
  }

  def updated(externs: Seq[ChunkEvent]): SWStateMonad = {
    // get groups of events produced by a particular chunk as well as the chunk that produced them
    val events: Seq[(Seq[ChunkEvent], Option[V3I])] =
      world.loaded.values.map(chunk => (chunk.update(world, world.time), Some(chunk.pos))).toSeq :+ (externs, None)

    // sort all events and integrate them into the world
    val newWorld: HashCacheWorld = world.integrate(events.flatMap(_._1).foldLeft(new TreeSet[ChunkEvent])(_ + _))

    // create a map of event-producer tuples sorted by their target
    val eventsByTarget: Map[V3I, Seq[(ChunkEvent, Option[V3I])]] =
      events.flatMap({ case (produced, producer) => produced.map(event => (event, producer)) }).groupBy(_._1.chunkPos)

    // compute which events need to be sent to which clients
    val eventsToSend: Map[ClientID, Set[ChunkEvent]] = subscriptions.mapValues({
      subscribed => subscribed.map(eventsByTarget.get).filter(_ isDefined).flatMap(_.get)
        .filterNot({ case (_, producer) => producer.exists(subscribed.contains) }).map(_._1)
    })

    // return
    SWStateMonad(
      copy(world = newWorld),
      SendEvents(eventsToSend.mapValues(_ toSeq))
    )
  }

}

case class SWStateMonad(
                       next: ServerWorld,
                       effects: Seq[SWSideEffect]
                       )

object SWStateMonad {

  def apply(next: ServerWorld, effect: SWSideEffect): SWStateMonad = SWStateMonad(next, Seq(effect))

}

sealed trait SWSideEffect
case class BufferPush(chunks: Seq[Chunk]) extends SWSideEffect
case class SendEvents(events: Map[ClientID, Seq[ChunkEvent]]) extends SWSideEffect

/**
  * Manages the server's world, save buffer, external events, chunk loading / unloading, and keeping track of what data
  * needs to be sent to which clients.
  *
  * Is based on imperative operations, but all methods are synchronized.
  *
  * Events that are integrated to the world at a certain point in time, are actually applied to the world between that
  * tick and the next tick. (Not between that tick and the previous tick).
  *
  * It known which clients are connected to the world, and which chunks each client is subscribing to at a particular
  * point in time.
  */
/*
class ServerWorldManager(save: WorldSave, generator: V3I => Block) extends AutoCloseable {

  val avatarLoadDistance = 4
  val bufferThickness = 4
  val historySize: Int = 5 * 60 // 5 seconds * 60 fps

  private val buffer = new SaveBuffer(save, generator)
  private val worldHistory = new mutable.TreeMap[Long, HashCacheWorld]
  worldHistory.put(0, HashCacheWorld(0))
  private val eventHistory = new mutable.TreeMap[Long, Map[V3I, Seq[(ChunkEvent, V3I)]]]
  private val externs = new mutable.TreeMap[Long, Seq[ChunkEvent]]
*/
  /*
  def getTime: Long = this.synchronized { history.lastKey }

  def getWorld: HashCacheWorld = this.synchronized { history.values.last }

  def getChunks(ps: Seq[V3I]): Map[V3I, Chunk] = this.synchronized {
    ps map (p => (p, getWorld chunkAt p)) filter (_._2 isDefined) map { case (p, option) => (p, option.get) } toMap
  }

  /**
    * Will not integrate events that occur in the future or before the earliest remembered time
    */
  def integrateExterns(newExterns: SortedMap[Long, Seq[ChunkEvent]]): (Long, SortedMap[Long, Map[ClientID, Seq[ChunkEvent]]]) =
    this.synchronized {
      val toIntegrate = newExterns.from(history.keys.head + 1)
      externs ++= toIntegrate
      val currTime = getTime
      while (history.lastKey >= toIntegrate.firstKey)
        history.dropRight(1)
      val accumulator = new mutable.TreeMap[Long, Map[ClientID, Seq[ChunkEvent]]]
      while (getTime < currTime) {
        val currTime = getTime
        accumulator.put(currTime, update(reloadify = false))
      }
      accumulator
    }

  def update(reloadify: Boolean = getTime % 300 == 0): Map[ClientID, Seq[ChunkEvent]] = this.synchronized {
    // pull the current world for modification
    var world = getWorld

    // calculate which clients should have which chunks loaded. assume the clients reached the same conclusion
    //TODO: move logic to strategy object and synchronize it between server and clients on startup
    //TODO: GEPS for if the client teleports into an unloaded chunk
    val clientLoadedMap: Map[ClientID, Seq[V3I]] = avatars.mapValues(avatar => {
      val pos = world.findEntity(avatar).asInstanceOf[Avatar].chunkPos
      val dist = avatarLoadDistance
      (pos - V3I(dist, dist, dist)) to (pos + V3I(dist, dist, dist))
    })

    // TODO: make this happen every tick, once the performance has been improved
    if (reloadify) {
      val shouldBeLoaded = clientLoadedMap.values.flatten

      // load needed chunks from buffer
      val needToLoad = shouldBeLoaded.filterNot(world.chunkIsDefinedAt)
      if (needToLoad nonEmpty)
        world ++= buffer.pull(needToLoad.toSeq).values.toSeq

      // unload chunks to buffer
      //TODO: replace this with something that will scale better with world size
      val shouldBeLoadedSet = shouldBeLoaded toSet
      val needToUnload = world.loaded.values.filterNot(chunk => shouldBeLoadedSet.contains(chunk.pos)).toSeq
      if (needToUnload nonEmpty) {
        buffer.push(needToUnload)
        world --= needToUnload.map(_.pos)
      }

      // update the buffer
      //TODO: make this able to happen in background threads
      buffer.update(world.loaded.keys.toSeq, bufferThickness, world)
    }

    // tuples of events and their producers
    val events: Seq[(Seq[ChunkEvent], Option[V3I])] =
      world.loaded.values.map(chunk => (chunk.update(world), Some(chunk.pos))).toSeq :+ (externs(getTime + 1), None)

    // TODO: for events applied to non-loaded chunks, push them into the save, and apply them upon loading
    // integrate the events into the world
    world = world.integrate(events.flatMap(_._1))

    // now that the world has been fully updated, manage history
    history.put(getTime + 1, world)
    if (history.size > historySize)
      history.drop(historySize - history.size)

    // the event-producer tuples, grouped by their targets
    val eventsByTarget: Map[V3I, Seq[(ChunkEvent, Option[V3I])]] =
      events.flatMap({ case (produced, producer) => produced.map(event => (event, producer)) }).groupBy(_._1.chunkPos)

    // compute which events need to be sent to which clients
    clientLoadedMap.mapValues(
      subscribed => subscribed.map(eventsByTarget.get).filter(_ isDefined).flatMap(_.get)
        .filterNot({ case (_, producer) => producer.exists(subscribed.contains) }).map(_._1)
    )
  }


  override def close(): Unit = {
    val world = getWorld
    buffer.push(world.loaded.values.toSeq)
    buffer.close(world)
  }



}
*/