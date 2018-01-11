package com.phoenixkahlo.hellcraft.service

import com.phoenixkahlo.hellcraft.util.threading.Fut

trait ServiceProcedure[S <: Service] {
  def begin(): Unit
  def apply[T](call: S#Call[T])(implicit exec: Runnable => Unit): Fut[T]
  def close(): Unit
}
