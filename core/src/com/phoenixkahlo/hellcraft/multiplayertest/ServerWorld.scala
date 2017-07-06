package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, World}
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.save.WorldSave

import scala.collection.{SortedSet, mutable}

/**
  * This represents a snapshot of a save file at a certain point in time. As the save file represents an infinitely
  * large world (the save encapsulates the generator), this class also represents an infinitely large world. However,
  * as an implementor of a core-logic interface, this must be immutable and thread-safe. The way this is achieved is
  * that, when a chunk is pushed to the save, this object first memoizes the previous version of the chunk.
  * If, at a later point, the original version of that same chunk is pushed to the save, the chunk will be un-memoized.
  * As such, the more the world save is mutated from this snapshot, the larger the snapshot's memory footprint will
  * grow. However, if the snapshot is made to produce its memoized chunks, and those chunks are pushed to the save,
  * the snapshot's memory footprint will collapse. For purposes of thread-safety, the save is used as the monitor.
  * The callbacks that this snapshot uses for listening to save-pushes are stored as weak references, so that if an
  * snapshot is otherwise discarded, it can be garbage collected, instead of accumulating memory footprint indefinitely.
  *
  * A thread that constructs a snapshot should synchonize to the save while doing so.
  */
//TODO: make locking smarter so that updating can be parallelized.
class ServerWorld(
                        save: WorldSave,
                        keepLoaded: Set[V3I],
                        provided: Map[V3I, Chunk],
                        val time: Long
                      ) extends World {

  // create a mutable memoization map from the provided chunks
  private val memoizations = new mutable.HashMap[V3I, Chunk] ++ provided
  // listen for save pushes
  save.weakListenForSave(chunk => {
    memoizations.get(chunk.pos) match {
      case None => memoizations.put(chunk.pos, chunk)
      case Some(memoized) => if (!keepLoaded.contains(chunk.pos) && memoized == chunk) memoizations -= chunk.pos
    }
  })
  // load chunks that should always be loaded and are not already loaded
  memoizations ++= save.load(keepLoaded.filterNot(provided.contains).toSeq)

  private val cache = new ThreadLocal[Chunk]

  override def chunkAt(chunkPos: V3I): Option[Chunk] = save.synchronized {
    val chunk = cache.get() match {
      case chunk if chunk != null && chunk.pos == chunkPos => Some(chunk)
      case _ => memoizations.get(chunkPos) match {
        case memoized if memoized isDefined => memoized
        case None => save.load(chunkPos)
      }
    }
    chunk.foreach(cache.set)
    chunk
  }

  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  override def findEntity(id: EventID): Entity = save.synchronized {
    memoizations.values.flatMap(_.entities).toMap.apply(id)
  }

  /**
    * While this is an imperative method, is does not actually externally mutate any objects.
    */
  def pushToSave(): Unit = save.synchronized {
    save.save(memoizations.values.toSeq, this)
  }

  def incrTime: ServerWorld = save.synchronized {
    new ServerWorld(save, keepLoaded, memoizations.toMap, time + 1)
  }

  def integrate(events: SortedSet[ChunkEvent]): ServerWorld = save.synchronized {
    // sort the events by their target
    val eventsByTarget = events.groupBy(_.target)
    // create the map of transformed chunks, not including memoized but non-transformed chunks
    val transformed: Map[V3I, Chunk] =
      eventsByTarget.map({
        case (target, group) => (target, group.foldLeft(chunkAt(target).get)({ case (chunk, event) => event(chunk) }))
      })
    // combine the memoized and transformed chunks to create a new snapshot
    new ServerWorld(
      save,
      keepLoaded,
      memoizations.toMap ++ transformed,
      time
    )
  }

}
