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
  implicit val type8 = CallService

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

class CallService[S <: Service, T](val call: S#Call[T], val onComplete: T => Seq[UpdateEffect], val service: ServiceTag[S])
  extends UpdateEffect with Serializable {
  override def effectType: UpdateEffectType[_ <: UpdateEffect] = CallService
}
object CallService extends UpdateEffectType[CallService[_ <: Service, _]](8) {
  def apply[S <: Service, T](call: S#Call[T], onComplete: T => Seq[UpdateEffect])(implicit service: ServiceTag[S]): CallService[S, T] =
    new CallService[S, T](call, onComplete, service)
}
