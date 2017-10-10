package com.phoenixkahlo.hellcraft.util.collections

trait ResourceNode {

  def dependencies: Seq[ResourceNode]

  def dispose(): Unit

}