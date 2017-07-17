package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.V3I

/**
  * A transformation that is applied to a particular chunk.
  */
abstract class ChunkEvent(val target: V3I, val id: UUID) extends Comparable[ChunkEvent] {

  def apply(chunk: Chunk): Chunk

  override def compareTo(o: ChunkEvent): Int =
    id compareTo o.id

  override def equals(o: Any): Boolean =
    o match {
      case event: ChunkEvent => id == event.id
      case _ => false
    }

  override def hashCode: Int =
    id.hashCode

}

case class PutBlock(at: V3I, block: Block, override val id: UUID) extends ChunkEvent(at / 16 floor, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putBlock(at % 16, block)
}

case class UncacheMesh(override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.renderUncached
}

case class PutEntity(override val target: V3I, entity: Entity, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putEntity(entity)
}

case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.removeEntity(entity)
}


/**
  * A transformation that is applied to a particular chunk.
  */
/*
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
*/