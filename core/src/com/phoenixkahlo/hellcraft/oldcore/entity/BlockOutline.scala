package com.phoenixkahlo.hellcraft.oldcore.entity

import java.util.UUID

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.graphics.old.RenderableFactory
import com.phoenixkahlo.hellcraft.oldcore._
import com.phoenixkahlo.hellcraft.graphics.{BlockOutlineModel, ResourcePack}
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.KeyParamPool

import scala.collection.JavaConverters

@CarboniteWith(classOf[FieldNode])
case class BlockOutline(override val pos: V3I, color: V3I, chunkSize: Int = 16) extends PositionHaver {

  override val id: UUID = UUID.randomUUID()

  override def update(world: World, ids: Stream[UUID], dt: Float) =
    Seq(RemoveEntity(chunkPos, id, ids.head))

  override def renderables(texturePack: ResourcePack): Seq[RenderableFactory] =
    Seq(BlockOutlineRenderer(pos, color.toColor))

}

object BlockOutline {

  def apply(pos: V3I, color: Color): BlockOutline =
    BlockOutline(pos, V3I(color))

}

case class BlockOutlineRenderer(v: V3I, color: Color) extends RenderableFactory {

  /**
    * Bring this object into an active state, generating resources, and return the renderables.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] = {
    // obtain instance
    val instance = new ModelInstance(BlockOutlineModel(color, color))
    instance.transform.setTranslation(v toGdx)
    // extract renderables from model
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    // return renderables
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Nil

}


