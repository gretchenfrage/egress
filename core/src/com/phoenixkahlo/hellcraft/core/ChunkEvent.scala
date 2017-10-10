package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{CubeFrame, Entity, Moveable}
import com.phoenixkahlo.hellcraft.graphics.SoundID
import com.phoenixkahlo.hellcraft.math.{Directions, RNG, V3F, V3I}
import com.phoenixkahlo.hellcraft.singleplayer.EntityID


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

/**
  * Flag for the driver that a chunk's terrain has been modified. Any chunk event that modifies a chunk's terrain
  * must emit this event.
  */
case class TerrainChanged(p: V3I) extends UpdateEffect {
  override def effectType: UpdateEffectType = TerrainChanged
}
case object TerrainChanged extends UpdateEffectType

// chunk events
abstract class ChunkEvent(val target: V3I, val id: UUID) extends UpdateEffect with Comparable[ChunkEvent] {
  def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect])

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
  * All events that change terrain must be marked with this trait.
  */
//trait TerrainChanger

@CarboniteFields
case class UpdateTerrain(neu: Terrain, override val id: UUID) extends ChunkEvent(neu.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.updateTerrain(neu), Seq(TerrainChanged(neu.pos)))
}

@CarboniteFields
case class PutEntity(entity: Entity, override val id: UUID) extends ChunkEvent(entity.chunkPos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.putEntity(entity), Seq.empty)
}

@CarboniteFields
case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.removeEntity(entity), Seq.empty)
}

@CarboniteFields
case class EffectDelayer(effect: UpdateEffect, override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk, Seq(effect))
}

@CarboniteFields
case class IncrDensity(v: V3F, override val id: UUID) extends ChunkEvent(v / 16 floor, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    val tc = (v / 16 * world.res floor) % world.res
    val densities = chunk.terrain.densities
    val nt = Densities(chunk.pos, chunk.terrain.materials, densities.updated(tc, densities(tc).get + 0.1f))
    (chunk, Seq(UpdateTerrain(nt, RNG(id.getLeastSignificantBits).nextUUID._2)))
  }
}

@CarboniteFields
case class IncrDensityCatalyst(v: V3F, override val id: UUID) extends ChunkEvent(v / 16 floor, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    val ids = RNG.uuids(RNG(id.getLeastSignificantBits))
    val events = Directions().map(_ + v).zip(ids).map({ case (v, id) => IncrDensity(v, id) })
    (chunk, events)
  }
}

abstract class UpdateEntity[T <: Entity](entityID: EntityID, override val target: V3I, override val id: UUID)
  extends ChunkEvent(target, id) {
  protected def update(entity: T): Entity

  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    chunk.entities.get(entityID) match {
      case Some(entity) =>
        val updated = update(entity.asInstanceOf[T])
        if (updated.chunkPos == entity.chunkPos) (chunk.putEntity(updated), Seq.empty)
        else (chunk.removeEntity(entityID), Seq(PutEntity(updated, RNG(id.getLeastSignificantBits).nextUUID._2)))
      case _ => (chunk, Seq.empty)
    }
  }
}

@CarboniteFields
case class ShiftEntity(dx: V3F, entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[Moveable](entityID, target, id) {
  override protected def update(entity: Moveable): Entity = entity.updatePos(entity.pos + dx)
}