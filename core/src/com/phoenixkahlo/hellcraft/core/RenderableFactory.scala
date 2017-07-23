package com.phoenixkahlo.hellcraft.core

import com.badlogic.gdx.graphics.g3d.Renderable

/**
  * The bridge between the purely functional logic core and the stateful OpenGL graphics system. Is a node in a
  * dependency graph.
  */
trait RenderableFactory {

  /**
    * Bring this object into an active state, generating resources, and return the renderables. While activating,
    * interpolate with the other world.
    */
  def apply(interpolate: Option[(World, Float)] = None): Seq[Renderable]

  def resources: Seq[ResourceNode]

}

trait ResourceNode {

  def dependencies: Seq[ResourceNode]

  def dispose(): Unit

}