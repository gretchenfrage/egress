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

  // a map of chunks which override the save, and a monitor for changing it
  // also, load the chunks that need to be kept loaded and were not provided
  private val monitor = new Object
  @volatile private var chunks: Map[V3I, Chunk] = provided ++ save.load(keepLoaded.filterNot(provided.contains).toSeq)
  private val cache = new ThreadLocal[Chunk]

  // when a chunk is pushed to the save, memoize it, or maybe even unmemoize it
  save.weakListenForSave(saving => {
    chunks.get(saving.pos) match {
      case None =>
        monitor.synchronized {
          chunks = chunks.updated(saving.pos, saving)
        }
      case Some(old) =>
        if (!keepLoaded.contains(saving.pos) && saving == old) monitor.synchronized {
          chunks -= saving.pos
        }
    }
  })


  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = true

  override def chunkAt(p: V3I): Option[Chunk] = save.synchronized {
    val cached = cache.get()
    if (cached != null && cached.pos == p) Some(cached)

    val chunk = chunks.get(p) match {
      case chunk if chunk isDefined => chunk
      case None => save.load(p)
    }
    chunk.foreach(cache.set)
    chunk
  }

  override def findEntity(id: EntityID): Option[Entity] = save.synchronized {
    chunks.values.flatMap(_.entities.get(id)).headOption
  }

  /**
    * While this is an imperative method, is does not actually externally mutate any objects.
    */
  def pushToSave(): Unit = save.synchronized {
    save.save(chunks.values.toSeq, this)
  }

  def incrTime: ServerWorld = save.synchronized {
    new ServerWorld(save, keepLoaded, chunks, time + 1)
  }

  def integrate(events: SortedSet[ChunkEvent]): ServerWorld = save.synchronized {
    val eventsByTarget: Map[V3I, SortedSet[ChunkEvent]] = events.groupBy(_.target)
    val transformed: Map[V3I, Chunk] = eventsByTarget.map({ case (target, group) =>
      (target, group.foldLeft(chunkAt(target).get)({ case (chunk, event) => event(chunk) }))
    })
    new ServerWorld(save, keepLoaded, chunks ++ transformed, time)
  }

  def setKeepLoaded(newKeepLoaded: Set[V3I]): ServerWorld = save.synchronized {
    new ServerWorld(save, newKeepLoaded, chunks, time)
  }

}