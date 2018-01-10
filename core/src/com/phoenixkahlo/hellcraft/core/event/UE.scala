package com.phoenixkahlo.hellcraft.core.event

import com.phoenixkahlo.hellcraft.core.entity.{AnyEnt, EntID, Entity}
import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.{MRNG, V3I}

/**
  * Update eval monad
  */
trait UE[+T] extends Serializable {
  def map[N](func: T => N): UE[N] = UE.UEMap(this, func)
  def flatMap[N](func: T => UE[N]): UE[N] = UE.UEFMap(this, func)
  def filter(test: T => Boolean): UE[T] = UE.UEFilter(this, test)
  def withFilter(test: T => Boolean): UE[T] = filter(test)
}

object UE {
  def apply[T](gen: => T): UE[T] = UEGen(() => gen)
  def chunk(p: V3I): UE[Option[Chunk]] = UEChunk(p)
  def terrain(p: V3I): UE[Option[Terrain]] = UETerrain(p)
  def ent[E <: Entity[_]](id: EntID[E]): UE[Option[E]] = UEEnt(id)
  def time: UE[Long] = UETime
  def rand: UE[MRNG] = UERand

  def chunks(ps: Seq[V3I]): UE[Seq[Chunk]] =
    ps.map(chunk).foldLeft(UE(Seq.empty[Chunk]))((a, b) => a.flatMap(aa => b.map(bb => aa ++ bb)))

  case class UEGen[T](fac: () => T) extends UE[T]
  case class UEMap[S, T](src: UE[S], func: S => T) extends UE[T]
  case class UEFMap[S, T](src: UE[S], func: S => UE[T]) extends UE[T]
  case class UEFilter[T](src: UE[T], test: T => Boolean) extends UE[T]
  case class UEChunk(p: V3I) extends UE[Option[Chunk]]
  case class UETerrain(p: V3I) extends UE[Option[Terrain]]
  case class UEEnt[T <: Entity[_]](id: EntID[T]) extends UE[Option[T]]
  case object UERand extends UE[MRNG]
  case object UETime extends UE[Long]
}