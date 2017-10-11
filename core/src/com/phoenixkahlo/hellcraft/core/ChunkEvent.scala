package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{CubeFrame, Entity, Moveable}
import com.phoenixkahlo.hellcraft.graphics.SoundID
import com.phoenixkahlo.hellcraft.math._
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

/*
@CarboniteFields
case class SetTerrain(neu: Terrain, override val id: UUID) extends ChunkEvent(neu.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.updateTerrain(neu), Seq(TerrainChanged(neu.pos)))
}
*/

@CarboniteFields
case class InvalidateTerrain(p: V3I, override val id: UUID) extends ChunkEvent(p, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.updateTerrain(chunk.terrain.toDensities), Seq(TerrainChanged(p)))
}

abstract class TerrainUpdater(p: V3I, override val id: UUID) extends ChunkEvent(p, id) {
  protected def update(chunk: Chunk, world: World): (Chunk, Set[V3I])

  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    val (updated, affected) = update(chunk, world)
    val invalidators = (affected - p).toSeq.zip(RNG.uuids(RNG(id.getLeastSignificantBits)))
      .map { case (pos, id) => InvalidateTerrain(pos, id) }
    (updated, TerrainChanged(p) +: invalidators)
  }
}

@CarboniteFields
case class Deposit(v: V3I, delta: Float, res: Int, override val id: UUID) extends TerrainUpdater(v / res floor, id) {
  override protected def update(chunk: Chunk, world: World): (Chunk, Set[V3I]) = {
    val affected = v.neighbors.map(_ / res floor).toSet
    val updated = chunk.updateTerrain(chunk.terrain.toDensities.incrDensity(v % res, delta))
    (updated, affected)
  }
}

@CarboniteFields
case class Flow(p: V3I, dir: Direction, override val id: UUID) extends TerrainUpdater(p, id) {
  override protected def update(chunk: Chunk, world: World): (Chunk, Set[V3I]) =
    (chunk.flow(world, dir), p.neighbors.toSet)
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
case class Later(effect: UpdateEffect, override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk, Seq(effect))
}

@CarboniteFields
case class Polarize(override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.computePolarity, Seq.empty)
}

/*
@CarboniteFields
case class Flow(override val target: V3I, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.flow(world),
    Seq(TerrainChanged(target)))
}

@CarboniteFields
case class Deposit(v: V3F, delta: Float, res: Int, override val id: UUID) extends ChunkEvent(v / res floor, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.updateTerrain(chunk.terrain.toDensities.incrDensity(v % res floor, delta)),
      Seq(TerrainChanged(target)))
}
*/

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
case class Shift(dx: V3F, entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[Moveable](entityID, target, id) {
  override protected def update(entity: Moveable): Entity = entity.updatePos(entity.pos + dx)
}