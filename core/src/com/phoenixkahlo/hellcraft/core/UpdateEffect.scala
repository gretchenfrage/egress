package com.phoenixkahlo.hellcraft.core

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.event.UE
import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval
import com.phoenixkahlo.hellcraft.core.request.{Request, Requested}
import com.phoenixkahlo.hellcraft.fgraphics.SoundID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.service.{Service, ServiceTag}

// type for all update effects
sealed trait UpdateEffect {
  def effectType: UpdateEffectType[_ <: UpdateEffect]
}
sealed case class UpdateEffectType[T <: UpdateEffect](ord: Int)
object UpdateEffectType {
  implicit val type0 = SoundEffect
  implicit val type1 = MakeRequest
  implicit val type2 = LogEffect
  implicit val type3 = PutChunk
  implicit val type4 = PutEnt
  implicit val type5 = RemEnt
  implicit val type6 = Event
  implicit val type7 = IdenEvent
  implicit val type8 = ServiceCall

  val types = Seq(type0, type1, type2, type3, type4, type5, type6, type7, type8)
}

// audio effects
case class SoundEffect(sound: SoundID, pow: Float, pos: V3F) extends UpdateEffect {
  override def effectType = SoundEffect
}
object SoundEffect extends UpdateEffectType[SoundEffect](0)

// request effect
case class MakeRequest[T](request: Request[T], onComplete: Requested => Seq[UpdateEffect]) extends UpdateEffect {
  override def effectType = MakeRequest
}
object MakeRequest extends UpdateEffectType[MakeRequest[_]](1)
// log effect
case class LogEffect(str: String) extends UpdateEffect {
  override def effectType = LogEffect
}
object LogEffect extends UpdateEffectType[LogEffect](2)

// chunk and entity put effects
case class PutChunk(c: Chunk) extends UpdateEffect {
  override def effectType = PutChunk
}
object PutChunk extends UpdateEffectType[PutChunk](3)

case class PutEnt(ent: AnyEnt) extends UpdateEffect {
  override def effectType = PutEnt
}
object PutEnt extends UpdateEffectType[PutEnt](4)

case class RemEnt(id: AnyEntID) extends UpdateEffect {
  override def effectType = RemEnt
}
object RemEnt extends UpdateEffectType[RemEnt](5)

//event effect
case class EventID(time: Long, phase: Byte, uuid: UUID) extends Comparable[EventID] {
  override def compareTo(other: EventID): Int =
    other.time - this.time match {
      case 0 => other.phase - this.phase match {
        case 0 => this.uuid compareTo other.uuid
        case b => b
      }
      case l if l > 0 => 1
      case _ => -1
    }
}

// version without ID, produced by world
case class Event(eval: UE[Seq[UpdateEffect]]) extends UpdateEffect {
  override def effectType = Event
  def iden(id: EventID) = IdenEvent(eval, id)
}
object Event extends UpdateEffectType[Event](6)

// version with ID, elevated by driver
case class IdenEvent(eval: UE[Seq[UpdateEffect]], id: EventID) extends UpdateEffect {
  override def effectType = IdenEvent
}
object IdenEvent extends UpdateEffectType[IdenEvent](7)

case class ServiceCall[S <: Service, T](cal: S#Call[T], onComplete: T => Seq[UpdateEffect], service: ServiceTag[S]) extends UpdateEffect {
  override def effectType: UpdateEffectType[_ <: UpdateEffect] = ServiceCall
}
object ServiceCall extends UpdateEffectType[ServiceCall[_ <: Service, _]](8)


// chunk events
/*
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
        case None => (chunk, Seq(LogEffect("entity event failed to find entity")))
      }
    })

  def shift[E <: Moveable[E]](p: V3I, entID: EntID[E], dx: V3F)(implicit rand: MRNG) =
    entEvent(p, entID)((ent, world) => ent.updatePos(ent.pos + dx))

  def physics(p: V3I, entID: EntID[PhysCube])(implicit rand: MRNG) =
    entEvent(p, entID)((ent, world) => ent.doPhysics(world))

}
*/