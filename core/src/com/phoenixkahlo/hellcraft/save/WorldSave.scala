package com.phoenixkahlo.hellcraft.save

import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I

trait WorldSave {

  def save(chunk: Chunk): Unit

  def load(chunkPos: V3I): Option[Chunk]

}
