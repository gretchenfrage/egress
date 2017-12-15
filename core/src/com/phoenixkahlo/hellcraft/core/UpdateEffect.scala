package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{Entity, Moveable, PhysCube}
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
case class SetTerrain(newTerrain: Terrain, override val id: UUID) extends ChunkEvent(newTerrain.pos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    chunk.setTerrain(newTerrain, RNG.uuids(RNG(id.hashCode())))
}
*/
case class FulfillChunk(p: V3I, requested: Requested, override val id: UUID) extends ChunkEvent(p, id) {
  override def apply(chunk: Chunk, world: World) =
    (chunk.fulfill(requested), Seq.empty)
}

case class InvalidateChunk(p: V3I, override val id: UUID, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true) extends ChunkEvent(p, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) =
    chunk.invalidate(RNG.uuids(RNG(id.hashCode())), meshTerrFast, meshBlocksFast)
}

case class SetMat(v: V3I, mat: TerrainUnit, override val id: UUID, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true) extends ChunkEvent(v / 16 floor, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = {
    var (c, e) = chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % 16, mat)), RNG.uuids(RNG(id.hashCode())), meshTerrFast, meshBlocksFast)
    e ++= (v.neighbors.map(_ / 16 floor).toSet - (v / 16 floor)).toSeq
      .zip(RNG.uuids(RNG(id.hashCode() ^ 0xF9302759)))
      .map({ case (p, id) => InvalidateChunk(p, id, meshTerrFast, meshBlocksFast) })
    (c, e)
  }
}

case class PutEntity(entity: Entity, override val id: UUID) extends ChunkEvent(entity.chunkPos, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.putEntity(entity), Seq.empty)
}

case class RemoveEntity(override val target: V3I, entity: UUID, override val id: UUID) extends ChunkEvent(target, id) {
  override def apply(chunk: Chunk, world: World): (Chunk, Seq[UpdateEffect]) = (chunk.removeEntity(entity), Seq.empty)
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

case class Shift(dx: V3F, entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[Moveable](entityID, target, id) {
  override protected def update(entity: Moveable, world: World): Entity = entity.updatePos(entity.pos + dx)
}

case class DoPhysics(entityID: EntityID, override val target: V3I, override val id: UUID)
  extends UpdateEntity[PhysCube](entityID, target, id) {
  override protected def update(entity: PhysCube, world: World): Entity = entity.doPhysics(world)
}