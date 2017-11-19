package com.phoenixkahlo.hellcraft.core.request

import java.util.UUID

import com.phoenixkahlo.hellcraft.util.collections.IdentityKey
import com.phoenixkahlo.hellcraft.util.threading.{Fut, MergeFut, UniExecutor}

import scala.collection.mutable

case class Request[T](id: UUID, eval: Evalable[T]) {
  def unlock(requested: Requested): Option[T] =
    if (requested.id == this.id) Some(requested.result.asInstanceOf[T])
    else None
}

class Requested(val id: UUID, private[request] val result: Any)

sealed trait Evalable[+T] {
  def map[R](func: T => R)(implicit exec: ExecHint): Evalable[R] = EMap(this, func, exec)

  def flatMap[R](func: T => Evalable[R]): Evalable[R] = EFlatMap(this, func)

  def toFut(accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]])(implicit executor: UniExecutor): Fut[T]
}

object Evalable {
  def apply[T](factory: => T)(implicit exec: ExecHint): Evalable[T] = ECreate(() => factory, exec)

  def merge[S1, S2, R](s1: Evalable[S1], s2: Evalable[S2], func: (S1, S2) => R)(implicit exec: ExecHint): Evalable[R] =
    EMerge(s1, s2, func, exec)
}

private case class ECreate[T](factory: () => T, exec: ExecHint) extends Evalable[T] {
  override def toFut(accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]])(implicit executor: UniExecutor) = {
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

private case class EMap[S, R](source: Evalable[S], func: S => R, exec: ExecHint) extends Evalable[R] {
  override def toFut(accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]])(implicit executor: UniExecutor) = {
    val k = IdentityKey(this)
    if (accum.contains(k))
      accum(k).asInstanceOf[Fut[R]]
    else {
      val fut = source.toFut(accum).map(func, exec.exec)
      accum.put(k, fut)
      fut
    }
  }
}

private case class EFlatMap[S, R](source: Evalable[S], func: S => Evalable[R]) extends Evalable[R] {
  override def toFut(accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]])(implicit executor: UniExecutor) = {
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[R]]
    } else {
      val fut = source.toFut(accum).flatMap(func(_).toFut(accum))
      accum.put(k, fut)
      fut
    }
  }
}

private case class EMerge[S1, S2, R](s1: Evalable[S1], s2: Evalable[S2], func: (S1, S2) => R, exec: ExecHint) extends Evalable[R] {
  override def toFut(accum: mutable.Map[IdentityKey[Evalable[_]], Fut[_]])(implicit executor: UniExecutor) = {
    val k = IdentityKey(this)
    if (accum.contains(k)) {
      accum(k).asInstanceOf[Fut[R]]
    } else {
      val fut = MergeFut(s1.toFut(accum), s2.toFut(accum), func(_, _))(exec.exec)
      accum.put(k, fut)
      fut
    }
  }
}