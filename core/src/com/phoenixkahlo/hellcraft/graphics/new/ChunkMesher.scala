package com.phoenixkahlo.hellcraft.graphics.`new`

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.graphics.{ResourcePack, StoneTID}
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.ParamCache

import scala.collection.mutable.ArrayBuffer

class ChunkMesher(chunk: Chunk) {

  val mesh = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    if (!chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      throw new IllegalArgumentException("chunk cannot render with undefined neighbors")

    val tris = chunk.terrain.quads(world).flatMap(_.decompose).flatMap(_.bothSides)

    val verts = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]

    val (u1, v1) = (pack(StoneTID).getU, pack(StoneTID).getV)
    val (u2, v2) = (pack(StoneTID).getU2, pack(StoneTID).getV)
    val (u3, v3) = (pack(StoneTID).getU2, pack(StoneTID).getV2)

    for (i <- tris.indices) {
      val tri = tris(i)
      indices.append((i * 3).toShort, (i * 3 + 1).toShort, (i * 3 + 2).toShort)
      verts.append(
        tri.a.x, tri.a.y, tri.a.z, Color.WHITE.toFloatBits, u1, v1,
        tri.b.x, tri.b.y, tri.b.z, Color.WHITE.toFloatBits, u2, v2,
        tri.c.x, tri.c.y, tri.c.z, Color.WHITE.toFloatBits, u3, v3
      )
    }

    val mesh = new Mesh(true, verts.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
    )

    mesh.setVertices(verts.toArray)
    mesh.setIndices(indices.toArray)

    val material = new Material
    material.set(TextureAttribute.createDiffuse(pack.sheet))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES

    val resource = new ResourceNode {
      override def dependencies: Seq[ResourceNode] = Seq.empty

      override def dispose(): Unit = mesh.dispose()
    }

    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        Seq(renderable)

      override def resources: Seq[ResourceNode] =
        Seq(resource)
    }
  }
  })

  def apply(world: World, pack: ResourcePack): Seq[RenderUnit] = {
    if (chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      Seq(mesh((world, pack)), new ChunkOutline(chunk.pos, Color.GREEN))
    else Seq.empty
  }

}
