package com.phoenixkahlo.hellcraft.core.request

import java.util.UUID

import com.phoenixkahlo.hellcraft.core.eval.WEval.WEval

case class Request[T](eval: WEval[T], id: UUID) {
  def unlock(requested: Requested): Option[T] =
    if (requested.id == this.id) Some(requested.result.asInstanceOf[T])
    else None
}

class Requested(val id: UUID, private[request] val result: Any)
