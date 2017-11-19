package com.phoenixkahlo.hellcraft.core.request

import com.phoenixkahlo.hellcraft.math.{V2F, V3F}
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

sealed trait ExecHint {
  def exec(task: Runnable)(implicit service: UniExecutor): Unit
}
case object ExecSeq extends ExecHint {
  override def exec(task: Runnable)(implicit service: UniExecutor): Unit = service.exec(task)
}
case class Exec3D(p: V3F) extends ExecHint {
  override def exec(task: Runnable)(implicit service: UniExecutor): Unit = service.exec(p)(task)
}
case class Exec2D(p: V2F) extends ExecHint {
  override def exec(task: Runnable)(implicit service: UniExecutor): Unit = service.exec(p)(task)
}