package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity.{Entity, Moveable, PhysCube}
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.fgraphics.SoundID
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

// request effect
case class MakeRequest[T](request: Request[T], onComplete: (Requested, World) => Seq[ChunkEvent]) extends UpdateEffect {
  override def effectType: UpdateEffectType = MakeRequest
}
case object MakeRequest extends UpdateEffectType

// chunk events
sealed trait ChunkEvent extends UpdateEffect {
  def id: UUID

  override def effectType: UpdateEffectType = ChunkEvent
}
case class UniChunkEvent(target: V3I, func: (Chunk, World) => (Chunk, Seq[UpdateEffect]), id: UUID) extends ChunkEvent
case class MultiChunkEvent(target: Set[V3I], func: (Map[V3I, Chunk], World) => (Map[V3I, Chunk], Seq[UpdateEffect]),
                           id: UUID) extends ChunkEvent
/*
case class ChunkEvent(target: Set[V3I], func: (Map[V3I, Chunk], World) => Map[V3I, Chunk], id: UUID) extends UpdateEffect
    with Comparable[ChunkEvent] {
  override def effectType: UpdateEffectType = ChunkEvent

  override def compareTo(o: ChunkEvent): Int = id compareTo o.id

  override def equals(other: Any): Boolean = other match {
    case ChunkEvent(_, _, oid) => oid == id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}
*/
case object ChunkEvent extends UpdateEffectType {
  def uni(target: V3I, func: (Chunk, World) => (Chunk, Seq[UpdateEffect]))(implicit rand: MRNG) =
    UniChunkEvent(target, func, rand.nextUUID)

  def multi(target: Set[V3I], func: (Map[V3I, Chunk], World) => (Map[V3I, Chunk], Seq[UpdateEffect]))(implicit rand: MRNG) =
    MultiChunkEvent(target, func, rand.nextUUID)

  def fulfill(p: V3I, requested: Requested)(implicit rand: MRNG) =
    uni(p, (chunk, world) => (chunk.fulfill(requested), Seq.empty))

  def invalidate(p: V3I, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true)(implicit rand: MRNG) =
    uni(p, (chunk, world) => chunk.invalidate(meshTerrFast, meshBlocksFast))

  def setMat(v: V3I, mat: TerrainUnit, meshTerrFast: Boolean = false, meshBlocksFast: Boolean = true)(implicit rand: MRNG) =
    uni(v / 16 floor, (chunk, world) => {
      val (c, e) = chunk.setTerrain(Terrain(chunk.pos, chunk.terrain.grid.updated(v % 16, mat)), meshTerrFast, meshBlocksFast)
      (c, e ++ (v.neighbors.map(_ / 16 floor).toSet - (chunk.pos)).toSeq.map(invalidate(_, meshTerrFast, meshBlocksFast)))
    })

  def putEnt(ent: Entity)(implicit rand: MRNG) =
    uni(ent.chunkPos, (chunk, world) => (chunk.putEntity(ent), Seq.empty))

  def remEnt(p: V3I, entID: EntityID)(implicit rand: MRNG) =
    uni(p, (chunk, world) => (chunk.removeEntity(entID), Seq.empty))

  def entEvent[E <: Entity](p: V3I, entID: EntityID, upd: (E, World) => E)(implicit rand: MRNG) =
    uni(p, (chunk, world) => {
      chunk.entities.get(entID) match {
        case Some(ent: E) =>
          val neu = upd(ent, world)
          if (neu.chunkPos == ent.chunkPos)
            (chunk.putEntity(neu), Seq.empty)
          else
            (chunk.removeEntity(entID), Seq(putEnt(neu)))
        case None => (chunk, Seq.empty)
      }
    })

  def shift(p: V3I, entID: EntityID, dx: V3F)(implicit rand: MRNG) =
    entEvent[Moveable](p, entID, (ent, world) => ent.updatePos(ent.pos + dx))

  def physics(p: V3I, entID: EntityID)(implicit rand: MRNG) =
    entEvent[PhysCube](p, entID, (ent, world) => ent.doPhysics(world))


  //def fulfillChunk(p: V3I, requested: Requested)(implicit rand: MRNG) =
  //  ChunkEvent(Set(p), (chunks, world) => chunks(p))
}


//case class ChunkEvent(target: V3I, func:)
/*
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
*/