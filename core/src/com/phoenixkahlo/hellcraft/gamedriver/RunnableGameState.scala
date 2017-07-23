package com.phoenixkahlo.hellcraft.gamedriver

trait RunnableGameState extends Runnable {

  def onEnter(): Unit

  def render(): Unit

  def onExit(): Unit

}
