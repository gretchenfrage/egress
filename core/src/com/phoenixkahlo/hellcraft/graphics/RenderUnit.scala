package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.util.ResourceNode

trait RenderUnit {

  def apply(interpolation: Interpolation): Seq[Renderable]

  def resources: Seq[ResourceNode]

}
