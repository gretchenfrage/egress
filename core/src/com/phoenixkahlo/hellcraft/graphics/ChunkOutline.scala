package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, Model, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.core.World
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.KeyParamPool

import scala.collection.JavaConverters

class ChunkOutlineRenderer(p: V3I, color: Color) extends RenderableFactory {
  /**
    * Bring this object into an active state, generating resources, and return the renderables. While activating,
    * interpolate with the other world.
    */
  override def apply(interpolate: Option[(World, Float)]): Seq[Renderable] = {
    // get model instance
    val instance = new ModelInstance(ChunkOutlineModel(color, color))
    instance.transform.setTranslation(p * 16 toGdx)
    // extract renderables from model
    val array = new com.badlogic.gdx.utils.Array[Renderable]()
    val pool = new Pool[Renderable]() {
      override def newObject(): Renderable = new Renderable
    }
    instance.getRenderables(array, pool)
    // return renderables
    JavaConverters.iterableAsScalaIterable(array).toSeq
  }

  override def resources: Seq[ResourceNode] = Seq.empty
}

object ChunkOutlineModel extends KeyParamPool[Color, Color, Model](color => {
  val n = 0.5f
  val p = 16 - 1e-3f
  val c = color.toFloatBits
  val verts = Array[Float](
    n, n, n, c,
    p, n, n, c,
    p, n, p, c,
    n, n, p, c,
    n, p, n, c,
    p, p, n, c,
    p, p, p, c,
    n, p, p, c
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