package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.math.V3I

/**
  * A transformation that is applied to a particular chunk.
  */
case class ChunkEvent(
                     chunkPos: V3I,
                     id: UUID,
                     event: Chunk => Chunk
                     ) extends Comparable[ChunkEvent] {

  def apply(chunk: Chunk): Chunk =
    event(chunk)

  override def compareTo(o: ChunkEvent): Int =
    id compareTo o.id

}