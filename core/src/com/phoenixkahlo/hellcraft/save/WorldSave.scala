package com.phoenixkahlo.hellcraft.save

import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.math.V3I

trait WorldSave extends AutoCloseable {

  def save(chunks: Seq[Chunk], world: World): Unit

  def save(chunk: Chunk, world: World): Unit

  def load(chunksPoss: Seq[V3I]): Map[V3I, Chunk]

  def load(chunkPos: V3I): Option[Chunk]

  def weakListenForSave(listener: (Chunk, Chunk) => Unit): Unit

  override def close(): Unit = {}

}
