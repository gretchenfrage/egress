package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders.{LineSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.caches.ParamCache
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

import scala.collection.JavaConverters

/*
case class CubeFrame(color: Color, override val pos: V3F, override val id: UUID) extends Entity {
  @transient private lazy val renderUnit = Seq(new CubeFrameRenderer(this))

  override def renderables(pack: ResourcePack): Seq[RenderUnit] = renderUnit
}

class CubeFrameRenderer(cube: CubeFrame) extends RenderUnit {
  override def apply(interpolation: Interpolation): Seq[Renderable] = {
    val instance = new ModelInstance(BlockOutlineModel(cube.color, cube.color))
    instance.transform.setTranslation(cube.pos toGdx)

    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    for (renderable <- JavaConverters.iterableAsScalaIterable(array)) {
      renderable.userData = LineSID
    }

    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}

*/