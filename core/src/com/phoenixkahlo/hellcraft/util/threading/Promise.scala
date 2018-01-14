package com.phoenixkahlo.hellcraft.util.threading

object Promise {
  def apply(gen: => Unit, exec: Runnable => Unit): Promise = Fut(gen, exec)
}
