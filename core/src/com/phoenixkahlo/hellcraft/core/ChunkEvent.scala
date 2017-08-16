package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, Cylindroid, Entity, PositionHaver}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

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

@CarboniteWith(classOf[FieldNode])
case class PutBlock(at: V3I, block: Block, override val id: UUID) extends ChunkEvent(at / 16 floor, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putBlock(at % 16, block)
}

@CarboniteWith(classOf[FieldNode])
case class UncacheMesh(override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.invalidateGraphics
}

@CarboniteWith(classOf[FieldNode])
case class AddEntity(entity: PositionHaver, override val id: UUID) extends ChunkEvent(entity.chunkPos, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putEntity(entity)
}

@CarboniteWith(classOf[FieldNode])
case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.removeEntity(entity)
}

object RemoveEntity {
  def apply(entity: PositionHaver, id: UUID): RemoveEntity = RemoveEntity(entity.chunkPos, entity.id, id)
}

@CarboniteWith(classOf[FieldNode])
case class ReplaceEntity(replacer: PositionHaver, override val id: UUID) extends ChunkEvent(replacer.chunkPos, id) {
  override def apply(chunk: Chunk): Chunk =
    if (chunk.entities.contains(replacer.id)) chunk.putEntity(replacer)
    else chunk
}

abstract class TransformEntity(entityID: UUID, override val target: V3I, override val id: UUID)
  extends ChunkEvent(target, id) {

  def transform(entity: Entity): Entity

  override def apply(chunk: Chunk): Chunk =
    chunk.entities.get(entityID) match {
      case Some(entity) => chunk.putEntity(transform(entity))
      case None => chunk
    }

}

@CarboniteWith(classOf[FieldNode])
case class SetAvatarMovement(
                              avatarID: UUID,
                              movDir: V3F,
                              jumping: Boolean,
                              sprinting: Boolean,
                              override val id: UUID,
                              override val target: V3I
                            ) extends TransformEntity(avatarID, target, id) {
  override def transform(entity: Entity): Entity =
    entity.asInstanceOf[Avatar].updateDirection(movDir).updateJumping(jumping).updateSprinting(sprinting)
}

@CarboniteWith(classOf[FieldNode])
case class ThrustCylindroid(
                       entityID: UUID,
                       deltaV: V3F,
                       override val target: V3I,
                       override val id: UUID
                       ) extends TransformEntity(entityID, target, id) {
  override def transform(entity: Entity): Entity = {
    val c = entity.asInstanceOf[Cylindroid[_ <: Cylindroid[_]]]
    c.updateVel(c.vel + deltaV)
  }
}