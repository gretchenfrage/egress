package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.fgraphics.SoundID
import com.phoenixkahlo.hellcraft.math._


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
case class MakeRequest[T](request: Request[T], onComplete: (Requested, World, MRNG) => Seq[ChunkEvent]) extends UpdateEffect {
  override def effectType: UpdateEffectType = MakeRequest
}
case object MakeRequest extends UpdateEffectType

// log effect
case class Log(str: String) extends UpdateEffect {
  override def effectType = Log
}
case object Log extends UpdateEffectType

// chunk events
sealed trait ChunkEvent extends UpdateEffect {
  def id: UUID

  override def effectType: UpdateEffectType = ChunkEvent
}
case class UniChunkEvent(target: V3I, func: (Chunk, World) => (Chunk, Seq[UpdateEffect]), id: UUID) extends ChunkEvent
case class MultiChunkEvent(target: Set[V3I], func: (Map[V3I, Chunk], World) => (Map[V3I, Chunk], Seq[UpdateEffect]),
                           id: UUID) extends ChunkEvent

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

  def putEnt(ent: AnyEnt)(implicit rand: MRNG) =
    uni(ent.chunkPos, (chunk, world) => (chunk.putEntity(ent), Seq.empty))

  def remEnt(p: V3I, entID: AnyEntID)(implicit rand: MRNG) =
    uni(p, (chunk, world) => (chunk.removeEntity(entID), Seq.empty))

  def entEvent[E <: Entity[E]](p: V3I, entID: EntID[E])(upd: (E, World) => E)(implicit rand: MRNG) =
    uni(p, (chunk, world) => {
      chunk.entities.get(entID) match {
        case Some(ent: E) =>
          val neu = upd(ent, world)
          if (neu.chunkPos == ent.chunkPos)
            (chunk.putEntity(neu), Seq.empty)
          else
            (chunk.removeEntity(entID), Seq(putEnt(neu)))
        case None => (chunk, Seq(Log("entity event failed to find entity")))
      }
    })

  def shift[E <: Moveable[E]](p: V3I, entID: EntID[E], dx: V3F)(implicit rand: MRNG) =
    entEvent(p, entID)((ent, world) => ent.updatePos(ent.pos + dx))

  def physics(p: V3I, entID: EntID[PhysCube])(implicit rand: MRNG) =
    entEvent(p, entID)((ent, world) => ent.doPhysics(world))

}
