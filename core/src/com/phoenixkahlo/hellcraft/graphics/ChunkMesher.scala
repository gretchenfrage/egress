package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.core.{Chunk, Facets, World}
import com.phoenixkahlo.hellcraft.graphics.shaders.SceneSID
import com.phoenixkahlo.hellcraft.math.{Origin, Quad, Tri}
import com.phoenixkahlo.hellcraft.util.ResourceNode
import com.phoenixkahlo.hellcraft.util.caches.{DisposableParamCache, ParamCache}

import scala.collection.mutable.ArrayBuffer

class ChunkMesher(chunk: Chunk, facets: Facets) {

  val mesh = new DisposableParamCache[(World, ResourcePack), Renderable]({ case (world, pack) => {
    val verts = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]

    val (u1, v1) = (pack(StoneTID).getU, pack(StoneTID).getV)
    val (u2, v2) = (pack(StoneTID).getU2, pack(StoneTID).getV)
    val (u3, v3) = (pack(StoneTID).getU2, pack(StoneTID).getV2)


    def proc(tris: Iterable[Tri]): Unit = for (tri <- tris) {
      val n = (tri.b - tri.a) cross (tri.c - tri.a) match {
        case Origin => Origin
        case v => v.normalize
      }

      indices.append(indices.length toShort, (indices.length + 1) toShort, (indices.length + 2) toShort)
      verts.append(
        tri.a.x, tri.a.y, tri.a.z, Color.RED.toFloatBits, u1, v1, n.x, n.y, n.z,
        tri.b.x, tri.b.y, tri.b.z, Color.GREEN.toFloatBits, u2, v2, n.x, n.y, n.z,
        tri.c.x, tri.c.y, tri.c.z, Color.BLUE.toFloatBits, u3, v3, n.x, n.y, n.z
      )
    }

    proc(facets.a)
    proc(facets.b)
    proc(facets.c)
    proc(facets.d)
    proc(facets.e)
    proc(facets.f)


    val mesh = new Mesh(true, verts.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )

    mesh.setVertices(verts.toArray)
    mesh.setIndices(indices.toArray)

    //val material = new Material
    //material.set(TextureAttribute.createDiffuse(pack.sheet))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
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

  /*
  val mesh = new DisposableParamCache[(World, ResourcePack), Renderable]({ case (world, pack) => {
    if (!chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      throw new IllegalArgumentException("chunk cannot render with undefined neighbors")

    val tris = quads.flatMap(_.decompose).flatMap(_.bothSides)

    val verts = new ArrayBuffer[Float]
    val indices = new ArrayBuffer[Short]

    val (u1, v1) = (pack(StoneTID).getU, pack(StoneTID).getV)
    val (u2, v2) = (pack(StoneTID).getU2, pack(StoneTID).getV)
    val (u3, v3) = (pack(StoneTID).getU2, pack(StoneTID).getV2)

    for (i <- tris.indices) {
      val tri = tris(i)
      val n = (tri.b - tri.a) cross (tri.c - tri.a) normalize

      indices.append((i * 3).toShort, (i * 3 + 1).toShort, (i * 3 + 2).toShort)
      verts.append(
        tri.a.x, tri.a.y, tri.a.z, Color.RED.toFloatBits, u1, v1, n.x, n.y, n.z,
        tri.b.x, tri.b.y, tri.b.z, Color.GREEN.toFloatBits, u2, v2, n.x, n.y, n.z,
        tri.c.x, tri.c.y, tri.c.z, Color.BLUE.toFloatBits, u3, v3, n.x, n.y, n.z
      )
    }

    val mesh = new Mesh(true, verts.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )

    mesh.setVertices(verts.toArray)
    mesh.setIndices(indices.toArray)

    //val material = new Material
    //material.set(TextureAttribute.createDiffuse(pack.sheet))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = new Material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indices.size
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
  */

}
