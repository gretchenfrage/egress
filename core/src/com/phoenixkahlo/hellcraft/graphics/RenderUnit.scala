package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.g3d.Renderable
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

trait RenderUnit {

  def apply(interpolation: Interpolation): Seq[Renderable]

  def resources: Seq[ResourceNode]

  def locationIfTransparent: Option[V3F] = None

}
