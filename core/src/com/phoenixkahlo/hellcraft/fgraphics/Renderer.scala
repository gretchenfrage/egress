package com.phoenixkahlo.hellcraft.fgraphics

import com.phoenixkahlo.hellcraft.graphics.ResourcePack

trait Renderer {
  def init(pack: ResourcePack): Unit
  def apply(renders: Seq[Render[_]]): Unit
  def close(): Unit
}
