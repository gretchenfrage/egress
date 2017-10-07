package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g3d.{ModelInstance, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics._
import com.phoenixkahlo.hellcraft.graphics.shaders.LineSID
import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.util.ResourceNode

import scala.collection.JavaConverters

@CarboniteWith(classOf[FieldNode])
case class Cube(color: Color, override val pos: V3F, override val id: UUID) extends Entity {
  @transient private lazy val renderUnit = Seq(new CubeRenderer(this))

  override def renderables(pack: ResourcePack): Seq[RenderUnit] = renderUnit
  /*
  override def update(world: World, ids: Stream[UUID], dt: Float): Seq[UpdateEffect] = {
    if (world.time % 60 == 0) Seq(SoundEffect(SnapSID, 1, pos))
    else Seq.empty
  }
  */
}
/*
class CubeRenderer(cube: Cube) extends RenderUnit {
  override def apply(interpolation: Interpolation): Seq[Renderable] = {
    val renderable = new Renderable
    val mesh = BlockOutlineModel(cube.color, cube.color)
    renderable.meshPart.mesh = mesh
    renderable.material = new com.badlogic.gdx.graphics.g3d.Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = mesh.getMaxIndices
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable.userData = LineSID
    Seq(renderable)
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}
*/

class CubeRenderer(cube: Cube) extends RenderUnit {
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
