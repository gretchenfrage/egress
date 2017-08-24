package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{Cube, Entity}
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

abstract class ChunkEvent(val target: V3I, val id: UUID) extends Comparable[ChunkEvent] {

  def apply(chunk: Chunk): Chunk

  override def compareTo(o: ChunkEvent): Int =
    id.compareTo(o.id)

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case event: ChunkEvent => id == event.id
      case _ => false
    }

  override def hashCode(): Int =
    id.hashCode()

}

@CarboniteWith(classOf[FieldNode])
case class AddEntity(entity: Entity, override val id: UUID) extends ChunkEvent(entity.chunkPos, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putEntity(entity)
}

@CarboniteWith(classOf[FieldNode])
case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.removeEntity(entity)
}

@CarboniteWith(classOf[FieldNode])
case class ReplaceEntity(replacer: Entity, override val id: UUID) extends ChunkEvent(replacer.chunkPos, id) {
  override def apply(chunk: Chunk): Chunk = {
    if (!chunk.entities.contains(replacer.id))
      throw new RuntimeException("no entity to replace")
    chunk.putEntity(replacer)
  }
}

object RemoveEntity {
  def apply(entity: Entity, id: UUID): RemoveEntity = RemoveEntity(entity.chunkPos, entity.id, id)
}

abstract class TransformEntity(entityID: UUID, override val target: V3I, override val id: UUID)
  extends ChunkEvent(target, id) {

  def transform(entity: Entity): Entity

  override def apply(chunk: Chunk): Chunk =
    chunk.putEntity(transform(chunk.entities(entityID)))
}

@CarboniteWith(classOf[FieldNode])
case class SetCubePos(cube: Cube, v: V3F, override val id: UUID) extends TransformEntity(cube.id, cube.chunkPos, id) {
  override def transform(entity: Entity): Entity =
    cube.copy(pos = v)
}