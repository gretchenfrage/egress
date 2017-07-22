package com.phoenixkahlo.hellcraft.save

import com.phoenixkahlo.hellcraft.core.{Block, Chunk, World}
import com.phoenixkahlo.hellcraft.math.V3I

/**
  * Wraps a generator and another save, and when a chunk is loaded, if the wrapped save does not produce it, it
  * will be generated.
  */
class GeneratingSave(wraps: WorldSave, generator: V3I => Block) extends WorldSave {

  override def save(chunks: Seq[Chunk], world: World): Unit =
    wraps.save(chunks, world)

  override def save(chunk: Chunk, world: World): Unit =
    wraps.save(chunk, world)

  override def load(chunkPoss: Seq[V3I]): Map[V3I, Chunk] = {
    val loaded = wraps.load(chunkPoss)
    chunkPoss.map(p => (p, loaded.getOrElse(p, new Chunk(p).mapBlocks(v => generator(p * 16 + v))))).toMap
  }

  override def load(chunkPos: V3I): Option[Chunk] =
    load(Seq(chunkPos)).get(chunkPos)

  override def weakListenForSave(listener: (Chunk, Chunk) => Unit): Unit =
    wraps.weakListenForSave(listener)

}
