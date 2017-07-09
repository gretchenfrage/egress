package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.math.V3I

/**
  * A transformation that is applied to a particular chunk.
  */
case class ChunkEvent(
                       target: V3I,
                       id: UUID,
                       event: Chunk => Chunk,
                       message: String = ""
                     ) extends Comparable[ChunkEvent] {

  def apply(chunk: Chunk): Chunk =
    event(chunk)

  override def compareTo(o: ChunkEvent): Int =
    id compareTo o.id

  /*
  override def equals(o: Any): Boolean =
    o match {
      case ChunkEvent(_, otherID, _) => id == otherID
      case _ => false
    }

  override def hashCode: Int =
    id.hashCode

*/

}