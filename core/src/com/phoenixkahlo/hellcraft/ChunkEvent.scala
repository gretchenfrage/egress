package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.util.V3I

/**
  * A transformation that is applied to a particular chunk.
  */
case class ChunkEvent(
                     chunkPos: V3I,
                     event: Chunk => Chunk
                     ) {

  def apply(chunk: Chunk) =
    event(chunk)

}