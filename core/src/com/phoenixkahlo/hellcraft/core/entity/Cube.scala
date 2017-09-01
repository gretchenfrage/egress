package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.{BlockOutlineModel, Interpolation, RenderUnit, ResourcePack}
import com.phoenixkahlo.hellcraft.graphics.RenderUnit
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.ResourceNode

import scala.collection.JavaConverters

@CarboniteWith(classOf[FieldNode])
case class Cube(color: Color, override val pos: V3F, override val id: UUID) extends Entity {
  override def renderables(pack: ResourcePack): Seq[RenderUnit] = {
    Seq(new CubeRenderer(this))
  }
}

class CubeRenderer(cube: Cube) extends RenderUnit {
  override def apply(interpolation: Interpolation): Seq[Renderable] = {
    val instance = new ModelInstance(BlockOutlineModel(cube.color, cube.color))
    instance.transform.setTranslation(cube.pos toGdx)

    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)

    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}