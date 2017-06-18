package com.phoenixkahlo.hellcraft.core

import com.badlogic.gdx.graphics.g3d.Renderable

/**
  * The bridge between the purely functional logic core and the stateful OpenGL graphics system. Is a node in a
  * dependency graph.
  */
trait RenderableFactory {

  /**
    * Bring this object into an active state, generating resources, and return the renderables.
    */
  def apply(): Seq[Renderable]

  /**
    * Return the sequence of factories that this factory depends on for being in an activate state.
    */
  def dependencies: Seq[RenderableFactory]

  /**
    * Bring this object into an unactive state, and dispose of resources.
    */
  def dispose(): Unit

}
