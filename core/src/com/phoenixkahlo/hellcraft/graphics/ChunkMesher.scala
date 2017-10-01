package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.core.{Chunk, Meshable, Vertices, World}
import com.phoenixkahlo.hellcraft.graphics.shaders.SceneSID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}

import scala.collection.mutable.ArrayBuffer

class ChunkMesher(chunk: Chunk, meshable: Meshable) {

  val mesh = new DisposableParamCache[(World, ResourcePack), Renderable]({ case (world, pack) => {
    val vertData = new ArrayBuffer[Float]

    val (u1, v1) = (pack(StoneTID).getU, pack(StoneTID).getV)
    val (u2, v2) = (pack(StoneTID).getU2, pack(StoneTID).getV)

    for (v: V3I <- meshable.vertMap) {
      val vert: Vertices.Vert = meshable.vertices(v).get

      val (texU, texV) = (u1, v1)

      vertData.append(
        vert.p.x, vert.p.y, vert.p.z,
        Color.WHITE.toFloatBits,
        texU, texV,
        vert.n.x, vert.n.y, vert.n.z
      )
    }

    val indexData = meshable.indices

    val mesh = new Mesh(true, vertData.size, indexData.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )

    mesh.setVertices(vertData.toArray)
    mesh.setIndices(indexData.toArray)

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexData.size
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable.userData = SceneSID
    renderable
  }}, _.meshPart.mesh.dispose())

  val meshUnit = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        Seq(mesh((world, pack)))

      override def resources: Seq[ResourceNode] =
        Seq(new ResourceNode {
          override def dependencies: Seq[ResourceNode] = Seq.empty

          override def dispose(): Unit = mesh.invalidate
        })
    }
  }})

  def apply(world: World, pack: ResourcePack): Seq[RenderUnit] = {
    if (chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      Seq(meshUnit((world, pack)))
    else Seq.empty
  }

}
