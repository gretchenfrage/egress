package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{CubeFrame, Entity, PhysicsCube}
import com.phoenixkahlo.hellcraft.graphics.SoundID
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

// type for all update effects
sealed trait UpdateEffect {
  def effectType: UpdateEffectType
}
sealed trait UpdateEffectType

// audio effects
case class SoundEffect(sound: SoundID, pow: Float, pos: V3F) extends UpdateEffect {
  override def effectType: UpdateEffectType = SoundEffect
}
case object SoundEffect extends UpdateEffectType

// chunk events
abstract sealed class ChunkEvent(val target: V3I, val id: UUID) extends UpdateEffect with Comparable[ChunkEvent] {
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

  override def effectType: UpdateEffectType = ChunkEvent
}
case object ChunkEvent extends UpdateEffectType

/**
  * This is the only event that can update terrain
  */
@CarboniteFields
case class UpdateTerrain(neu: Terrain, override val id: UUID) extends ChunkEvent(neu.pos, id) {
  override def apply(chunk: Chunk): Chunk = chunk.updateTerrain(neu)
}

@CarboniteFields
case class AddEntity(entity: Entity, override val id: UUID) extends ChunkEvent(entity.chunkPos, id) {
  override def apply(chunk: Chunk): Chunk = chunk.putEntity(entity)
}

@CarboniteFields
case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk): Chunk = chunk.removeEntity(entity)
}

@CarboniteFields
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

@CarboniteFields
case class UpdatePhysCubePosVel(cube: PhysicsCube, newPos: V3F, newVel: V3F,
                                override val id: UUID) extends TransformEntity(cube.id, cube.chunkPos, id)  {
  override def transform(entity: Entity): Entity =
    entity.asInstanceOf[PhysicsCube].copy(pos = newPos, vel = newVel)
}

object UpdatePhysCubePosVel {
  def apply(cube: PhysicsCube, newPos: V3F, newVel: V3F, ids: Stream[UUID]): Seq[ChunkEvent] = {
    if (cube.chunkPos == (newPos / 16 floor)) Seq(UpdatePhysCubePosVel(cube, newPos, newVel, ids.head))
    else Seq(
      RemoveEntity(cube, ids.head),
      AddEntity(cube.copy(pos = newPos, vel = newVel), ids.drop(1).head)
    )
  }
}