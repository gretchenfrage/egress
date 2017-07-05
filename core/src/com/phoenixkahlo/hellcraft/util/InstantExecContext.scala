package com.phoenixkahlo.hellcraft.util

import scala.concurrent.ExecutionContext

object InstantExecContext extends ExecutionContext {

  override def execute(runnable: Runnable): Unit = runnable.run()

  override def reportFailure(cause: Throwable): Unit = ???

}
