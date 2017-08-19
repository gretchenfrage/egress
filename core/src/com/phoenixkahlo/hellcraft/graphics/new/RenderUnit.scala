package com.phoenixkahlo.hellcraft.graphics.`new`

import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.graphics.ResourceNode

trait RenderUnit {

  def apply(interpolation: Interpolation): Seq[Renderable]

  def resources: Seq[ResourceNode]

}
