package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.core.CompleteTerrain.Vert
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.shaders.{LineSID, PointSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

import scala.collection.mutable.ArrayBuffer

class ChunkMesher(chunk: Chunk, meshable: CompleteTerrain) {

  val mesh = new DisposableParamCache[(ResourcePack, World), Renderable]({ case (pack, world) => {
    val vertData = new ArrayBuffer[Float]

    for (v: V3I <- meshable.indexToVert) {
      val vert: Vert = meshable.verts(v).get

      val mat = vert.mat
      val tex = pack(mat.tid)
      val (texU, texV) = (tex.getU, tex.getV)

      vertData.append(
        vert.pos.x, vert.pos.y, vert.pos.z,
        Color.WHITE.toFloatBits,
        texU, texV,
        Float.NaN, Float.NaN, Float.NaN
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
    renderable.userData = TerrainSID
    renderable
  }}, _.meshPart.mesh.dispose())


  val meshUnit = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        Seq(mesh((pack, world)))
      
      override val resources: Seq[ResourceNode] =
        Seq(new ResourceNode {
          override def dependencies: Seq[ResourceNode] = Seq.empty

          override def dispose(): Unit = {
            mesh.invalidate
          }
        })
    }
  }})

  def apply(world: World, pack: ResourcePack): Seq[RenderUnit] = {
    if (chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      Seq(meshUnit((world, pack)))
    else Seq.empty
  }

}
