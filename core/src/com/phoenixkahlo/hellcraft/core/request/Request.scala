package com.phoenixkahlo.hellcraft.core.request

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.ExecHint
import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval
import com.phoenixkahlo.hellcraft.core.{Chunk, Terrain}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.collections.IdentityKey
import com.phoenixkahlo.hellcraft.util.threading._

import scala.collection.mutable

case class Request[T](eval: WEval[T], id: UUID) {
  def unlock(requested: Requested): Option[T] =
    if (requested.id == this.id) Some(requested.result.asInstanceOf[T])
    else None
}

class Requested(val id: UUID, private[request] val result: Any)

/*
sealed trait Evalable[+T] {
  def map[R](func: T => R)(implicit exec: ExecHint): Evalable[R] = EMap(this, func, exec)

  def flatMap[R](func: T => Evalable[R]): Evalable[R] = EFlatMap(this, func)

  def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]] = new mutable.HashMap): Fut[T]
}

object Evalable {
  case class ToFutPack(executor: UniExecutor, cfulfill: FulfillmentContext[V3I, Chunk], tfulfill: FulfillmentContext[V3I, Terrain])

  def apply[T](factory: => T)(implicit exec: ExecHint): Evalable[T] = ECreate(() => factory, exec)

  def merge[S1, S2, R](s1: Evalable[S1], s2: Evalable[S2], func: (S1, S2) => R)(implicit exec: ExecHint): Evalable[R] =
    EMerge(s1, s2, func, exec)

  def chunk(p: V3I): Evalable[Chunk] = EGetChunk(p)

  def terrain(p: V3I): Evalable[Terrain] = EGetTerrain(p)
}

private case class ECreate[T](factory: () => T, exec: ExecHint) extends Evalable[T] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[T] = {
    implicit val executor = pack.executor
    val k = IdentityKey(this)
    if (accum.contains(k))
      accum(k).asInstanceOf[Fut[T]]
    else {
      val fut = Fut(factory(), exec.exec)
      accum.put(k, fut)
      fut
    }
  }
}

private case class EGetChunk(v: V3I) extends Evalable[Chunk] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[Chunk] = {
    implicit val context = pack.cfulfill
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[Chunk]]
    } else {
      val fut = FulfillFut(v)
      accum.put(k, fut)
      fut
    }
  }
}

private case class EGetTerrain(v: V3I) extends Evalable[Terrain] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[Terrain] = {
    implicit val context = pack.tfulfill
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[Terrain]]
    } else {
      val fut = FulfillFut(v)
      accum.put(k, fut)
      fut
    }
  }
}

private case class EMap[S, R](source: Evalable[S], func: S => R, exec: ExecHint) extends Evalable[R] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[R] = {
    implicit val executor = pack.executor
    val k = IdentityKey(this)
    if (accum.contains(k))
      accum(k).asInstanceOf[Fut[R]]
    else {
      val fut = source.toFut(pack, accum).map(func, exec.exec)
      accum.put(k, fut)
      fut
    }
  }
}

private case class EFlatMap[S, R](source: Evalable[S], func: S => Evalable[R]) extends Evalable[R] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[R] = {
    implicit val executor = pack.executor
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[R]]
    } else {
      val fut = source.toFut(pack, accum).flatMap(func(_).toFut(pack, accum))
      accum.put(k, fut)
      fut
    }
  }
}

private case class EMerge[S1, S2, R](s1: Evalable[S1], s2: Evalable[S2], func: (S1, S2) => R, exec: ExecHint) extends Evalable[R] {
  override def toFut(pack: ToFutPack, accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]]): Fut[R] = {
    implicit val executor = pack.executor
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[R]]
    } else {
      val fut = MergeFut(s1.toFut(pack, accum), s2.toFut(pack, accum), func(_, _))(exec.exec)
      accum.put(k, fut)
      fut
    }
  }
}
*/