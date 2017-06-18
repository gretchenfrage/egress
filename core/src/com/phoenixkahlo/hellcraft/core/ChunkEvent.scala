package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math.V3I

/**
  * A transformation that is applied to a particular chunk.
  */
case class ChunkEvent(
                     chunkPos: V3I,
                     event: Chunk => Chunk
                     ) {

  def apply(chunk: Chunk): Chunk =
    event(chunk)

}