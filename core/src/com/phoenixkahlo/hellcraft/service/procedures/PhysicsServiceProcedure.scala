package com.phoenixkahlo.hellcraft.service.procedures

import com.phoenixkahlo.hellcraft.service.PhysicsService.{Act, Body}
import com.phoenixkahlo.hellcraft.service.{PhysicsService, ServiceProcedure}
import com.phoenixkahlo.hellcraft.util.threading.Fut

class PhysicsServiceProcedure extends ServiceProcedure[PhysicsService] {
  override def begin(): Unit = ()

  def act(body: Body)(implicit exec: (Runnable) => Unit): Fut[Body] = Fut(body, exec)

  override def apply[T](call: PhysicsService.Call[T])(implicit exec: (Runnable) => Unit): Fut[T] = call match {
    case Act(body) => act(body).asInstanceOf[Fut[T]]
  }

  override def close(): Unit = ()
}
