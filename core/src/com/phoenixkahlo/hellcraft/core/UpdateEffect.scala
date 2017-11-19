package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.{CarboniteFields, CarboniteWith}
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.entity.{CubeFrame, Entity, Moveable, PhysCube}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
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

// request events
case class MakeRequest[T](request: Request[T], onComplete: (Requested, World) => Seq[ChunkEvent]) extends UpdateEffect {
  override def effectType: UpdateEffectType = MakeRequest
}
case object MakeRequest extends UpdateEffectType

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

@CarboniteFields
case class SetTerrain(neu: Terrain, override val id: UUID) extends ChunkEvent(neu.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.setTerrain(neu), Seq(TerrainChanged(neu.pos)))
}

@CarboniteFields
case class SetTerrainSoup(ts: TerrainSoup, override val id: UUID) extends ChunkEvent(ts.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.setTerrainSoup(ts), Seq.empty)
}

@CarboniteFields
case class SetBlockSoup(bs: BlockSoup, override val id: UUID) extends ChunkEvent(bs.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (chunk.setBlockSoup(bs), Seq.empty)
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
case class Invalidate(p: V3I, override val id: EntityID, revalTerrain: Boolean = false, revalBlocks: Boolean = false)
  extends ChunkEvent(p, id) {

  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    var c = chunk.invalidate
    if (revalTerrain)
      for (soup <- TerrainSoup(c.terrain, world))
        c = c.setTerrainSoup(soup)
    if (revalBlocks)
      for (soup <- BlockSoup(c.terrain, world))
        c = c.setBlockSoup(soup)
    (c, Seq(TerrainChanged(p)))
  }
}

@CarboniteFields
case class SetMat(v: V3I, mat: TerrainUnit, res: Int, override val id: UUID,
                  revalTerrain: Boolean = false, revalBlocks: Boolean = false) extends ChunkEvent(v / res floor, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    (
      chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % res, mat))),
      (v - V3I(2, 2, 2) to v + V3I(2, 2, 2)).map(_ / res floor).toSet.toSeq
        .zip(RNG.uuids(RNG(id.getLeastSignificantBits)))
        .map({ case (p, id) => Invalidate(p, id, revalTerrain, revalBlocks) })
    )
}

abstract class UpdateEntity[T <: Entity](entityID: EntityID, override val target: V3I, override val id: UUID)
  extends ChunkEvent(target, id) {
  protected def update(entity: T, world: World): Entity

  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    chunk.entities.get(entityID) match {
      case Some(entity) =>
        val updated = update(entity.asInstanceOf[T], world)
        if (updated.chunkPos == entity.chunkPos) (chunk.putEntity(updated), Seq.empty)
        else (chunk.removeEntity(entityID), Seq(PutEntity(updated, RNG(id.getLeastSignificantBits).nextUUID._2)))
      case _ => (chunk, Seq.empty)
    }
  }
}

@CarboniteFields
case class Shift(dx: V3F, entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[Moveable](entityID, target, id) {
  override protected def update(entity: Moveable, world: World): Entity = entity.updatePos(entity.pos + dx)
}

@CarboniteFields
case class DoPhysics(entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[PhysCube](entityID, target, id) {
  override protected def update(entity: PhysCube, world: World): Entity = entity.doPhysics(world)
}