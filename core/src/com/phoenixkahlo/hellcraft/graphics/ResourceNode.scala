package com.phoenixkahlo.hellcraft.graphics

trait ResourceNode {

  def dependencies: Seq[ResourceNode]

  def dispose(): Unit

}