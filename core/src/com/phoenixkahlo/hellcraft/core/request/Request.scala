package com.phoenixkahlo.hellcraft.core.request

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval

case class Request[T](eval: WEval[T], id: UUID) {
  def unlock(requested: Requested): Option[T] =
    if (requested.id == this.id) Some(requested.result.asInstanceOf[T])
    else None
}

class Requested(val id: UUID, private[request] val result: Any) extends Serializable {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Requested]

  override def equals(other: Any): Boolean = other match {
    case that: Requested =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}