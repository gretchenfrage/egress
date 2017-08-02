package com.phoenixkahlo.hellcraft.core.entity

import java.util.UUID

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.KeyParamPool

import scala.collection.JavaConverters

case class BlockOutline(override val pos: V3I, color: Color, chunkSize: Int = 16) extends PositionHaver {

  override val id: UUID = UUID.randomUUID()

  override def update(world: World, ids: Stream[UUID]): Seq[ChunkEvent] =
    Seq(RemoveEntity(chunkPos, id, ids.head))

  override def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    Seq(BlockOutlineRenderer(pos, color))

}

case class BlockOutlineRenderer(v: V3I, color: Color) extends RenderableFactory {

  /**
    * Bring this object into an active state, generating resources, and return the renderables.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] = {
    // obtain instance
    val instance = new ModelInstance(OutlineModelPool(color, color))
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

object OutlineModelPool extends KeyParamPool[Color,Color,Model](color => {
  val n = 0 - 1e-3f
  val p = 1 + 1e-3f
  val verts: Array[Float] = Array[Float](
    n, n, n, color toFloatBits,
    p, n, n, color toFloatBits,
    p, n, p, color toFloatBits,
    n, n, p, color toFloatBits,
    n, p, n, color toFloatBits,
    p, p, n, color toFloatBits,
    p, p, p, color toFloatBits,
    n, p, p, color toFloatBits
  )
  val indices: Array[Short] = Array[Short](
    0, 1,
    1, 2,
    2, 3,
    3, 0,
    4, 5,
    5, 6,
    6, 7,
    7, 4,
    0, 4,
    1, 5,
    2, 6,
    3, 7
  )

  val mesh = new Mesh(true, 8, 24,
    new VertexAttribute(Usage.Position, 3, "a_position"),
    new VertexAttribute(Usage.ColorPacked, 4, "a_color")
  )

  mesh.setVertices(verts)
  mesh.setIndices(indices)

  val material = new Material()
  material.set(ColorAttribute.createDiffuse(color))

  val meshPart = new MeshPart("outline", mesh, 0, mesh.getNumIndices, GL20.GL_LINES)

  val builder = new ModelBuilder()
  builder.begin()
  builder.part(meshPart, material)
  builder.end()

})
