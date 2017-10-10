package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.graphics.shaders.{LineSID, PointSID, TerrainSID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}
import com.phoenixkahlo.hellcraft.util.collections.ResourceNode

import scala.collection.mutable.ArrayBuffer

class ChunkMesher(chunk: Chunk, meshable: Meshable) {

  val mesh = new DisposableParamCache[(ResourcePack, World), Renderable]({ case (pack, world) => {
    val vertData = new ArrayBuffer[Float]

    //val (u1, v1) = (pack(StoneTID).getU, pack(StoneTID).getV)
    //val (u2, v2) = (pack(StoneTID).getU2, pack(StoneTID).getV)

    for (v: V3I <- meshable.vertMap) {
      val vert: Vertices.Vert = meshable.vertices(v).get

      //val (texU, texV) = (u1, v1)
      //val mat = world.materialGridPoint(chunk.pos * world.res + v).get
      val mat = vert.material
      val tex = pack(mat.tid)
      val (texU, texV) = (tex.getU, tex.getV)

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
    renderable.userData = TerrainSID
    renderable
  }}, _.meshPart.mesh.dispose())

  val gradient = new DisposableParamCache[Unit, Renderable](u => {
    val vertices = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]
    var index = 0

    for (vert <- meshable.vertices) {
      val d = vert.p + vert.n
      vertices.append(
        vert.p.x, vert.p.y, vert.p.z, Color.BLACK.toFloatBits,
        d.x, d.y, d.z, Color.WHITE.toFloatBits
      )
      indices.append(index.toShort, (index + 1).toShort)
      index += 2
    }

    val mesh = new Mesh(true, vertices.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color")
    )

    mesh.setVertices(vertices.toArray)
    mesh.setIndices(indices.toArray)

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
    renderable.meshPart.primitiveType = GL20.GL_LINES
    renderable.userData = LineSID

    renderable
  }, _.meshPart.mesh.dispose())

  val density = new DisposableParamCache[World, Renderable](world => {
    val vertices = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]
    var index = 0

    for (v <- (chunk.pos * 16) until ((chunk.pos + Ones) * 16)) {
      vertices.append(v.x, v.y, v.z, new Color(world.sampleDensity(v).get, 0, 0, 1).toFloatBits)
      indices.append(index.toShort)
      index += 1
    }

    val mesh = new Mesh(true, vertices.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color")
    )

    mesh.setVertices(vertices.toArray)
    mesh.setIndices(indices.toArray)

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
    renderable.meshPart.primitiveType = GL20.GL_POINTS
    renderable.userData = PointSID

    renderable
  }, _.meshPart.mesh.dispose())

  val meshUnit = new ParamCache[(World, ResourcePack), RenderUnit]({ case (world, pack) => {
    new RenderUnit {
      override def apply(interpolation: Interpolation): Seq[Renderable] =
        Seq(mesh((pack, world)))
        //Seq(mesh(pack), gradient(()))
        //Seq(mesh(pack), gradient(()), density(world))

      override val resources: Seq[ResourceNode] =
        Seq(new ResourceNode {
          override def dependencies: Seq[ResourceNode] = Seq.empty

          override def dispose(): Unit = {
            mesh.invalidate
            gradient.invalidate
            density.invalidate
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
