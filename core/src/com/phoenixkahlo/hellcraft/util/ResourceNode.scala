package com.phoenixkahlo.hellcraft.util

trait ResourceNode {

  def dependencies: Seq[ResourceNode]

  def dispose(): Unit

}