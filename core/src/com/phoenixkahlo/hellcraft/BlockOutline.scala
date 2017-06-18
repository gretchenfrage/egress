package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.{V3F, V3I}

import scala.collection.JavaConverters

case class BlockOutline(v: V3I, color: Color, chunkSize: Int = 16) extends Entity {

  val chunkPos: V3I = v / chunkSize floor

  override val id: UUID = UUID.randomUUID()

  override def update(world: World): Seq[ChunkEvent] =
    Seq(ChunkEvent(chunkPos, _.removeEntity(this)))

  override def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    Seq(BlockOutlineRenderer(v, color))

}

case class BlockOutlineRenderer(v: V3I, color: Color) extends RenderableFactory {

  override def apply(): Seq[Renderable] = {
    val verts: Array[Float] = Array[Float](
      v.x + 0, v.y + 0, v.z + 0, color toFloatBits,
      v.x + 1, v.y + 0, v.z + 0, color toFloatBits,
      v.x + 1, v.y + 0, v.z + 1, color toFloatBits,
      v.x + 0, v.y + 0, v.z + 1, color toFloatBits,
      v.x + 0, v.y + 1, v.z + 0, color toFloatBits,
      v.x + 1, v.y + 1, v.z + 0, color toFloatBits,
      v.x + 1, v.y + 1, v.z + 1, color toFloatBits,
      v.x + 0, v.y + 1, v.z + 1, color toFloatBits
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

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = 24
    renderable.meshPart.primitiveType = GL20.GL_LINES
    Seq(renderable)
  }

}
