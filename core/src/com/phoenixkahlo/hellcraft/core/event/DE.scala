package com.phoenixkahlo.hellcraft.core.event

import com.phoenixkahlo.hellcraft.core.entity.{EntID, Entity}
import com.phoenixkahlo.hellcraft.core.{Chunk, Event, MakeRequest, Terrain, UpdateEffect}
import com.phoenixkahlo.hellcraft.core.eval.{ExecCheap, WEval}
import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval
import com.phoenixkahlo.hellcraft.core.request.Request
import com.phoenixkahlo.hellcraft.math.V3I

// delayable eval monads, which compile into a combination of UE monads and WEval monads
trait DE[+T] extends Serializable {
  def map[N](func: T => N): DE[N] = DE.DEMap(this, func)
  def flatMap[N](func: T => DE[N]): DE[N] = DE.DEFMap(this, func)
  def filter(test: T => Boolean): DE[T] = DE.DEFilter(this, test)
  def withFilter(test: T => Boolean): DE[T] = filter(test)

  def process: UE[Either[T, Seq[WEval[_]]]]
}

object Delayable {
  def apply(de: DE[Seq[UpdateEffect]]): Event =
    Event(de.process.map({
      case Left(effects) => effects
      case Right(dependents) =>
        implicit val hint = ExecCheap
        val merged = dependents.map(_.map(a => ())).reduce((a, b) => a.flatMap(aa => b))
        Seq(MakeRequest(Request(merged), result => Seq(Delayable(de))))
    }))
}

object DE {
  def apply[T](gen: => T): DE[T] = DEGen(() => gen)
  def chunk(p: V3I): DE[Chunk] = DEChunk(p)
  def terrain(p: V3I): DE[Terrain] = DETerrain(p)
  def ent[E <: Entity[_]](id: EntID[E]): DE[E] = DEEnt(id)

  case class DEGen[T](fac: () => T) extends DE[T] {
    override def process: UE[Either[T, Seq[WEval[_]]]] = UE(Left(fac()))
  }
  case class DEMap[S, T](src: DE[S], func: S => T) extends DE[T] {
    override def process: UE[Either[T, Seq[WEval[_]]]] = src.process.map({
      case Left(s) => Left(func(s))
      case right@Right(dep) => right.asInstanceOf[Right[T, Seq[WEval[_]]]]
    })
  }
  case class DEFMap[S, T](src: DE[S], func: S => DE[T]) extends DE[T] {
    override def process: UE[Either[T, Seq[WEval[_]]]] =
      src.process.flatMap({
        case Left(s) => func(s).process
        case right@Right(dep) => UE(right.asInstanceOf[Right[T, Seq[WEval[_]]]])
      })
  }
  case class DEFilter[T](src: DE[T], test: T => Boolean) extends DE[T] {
    override def process: UE[Either[T, Seq[WEval[_]]]] =
      src.process.filter({
        case Left(t) => test(t)
        case Right(dep) => true
      })
  }
  case class DEChunk(p: V3I) extends DE[Chunk] {
    override def process: UE[Either[Chunk, Seq[WEval[_]]]] =
      UE.chunk(p).map({
        case Some(chunk) => Left(chunk)
        case None => Right(Seq(WEval.chunk(p)))
      })
  }
  case class DETerrain(p: V3I) extends DE[Terrain] {
    override def process: UE[Either[Terrain, Seq[WEval[_]]]] =
      UE.terrain(p).map({
        case Some(terr) => Left(terr)
        case None => Right(Seq(WEval.terrain(p)))
      })
  }
  case class DEEnt[T <: Entity[_]](id: EntID[T]) extends DE[T] {
    override def process: UE[Either[T, Seq[WEval[_]]]] =
      UE.ent(id).map({
        case Some(ent) => Left(ent)
        case None => Right(Seq(WEval.ent(id)))
      })
  }
}
