package com.phoenixkahlo.hellcraft
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.phoenixkahlo.hellcraft.util._

import scala.collection.immutable.{HashMap, HashSet}

case class ChunkRenderer(chunk: Chunk) extends RenderableFactory {

  lazy val cache: Renderable = {

    // first, compute the exposed surfaces
    type SurfaceMap = Map[Direction,Set[V3I]]
    // compute the given surface of the given block
    def surface(m: SurfaceMap, v: V3I, s: Direction): SurfaceMap = {
      (chunk(v), chunk(v + s)) match {
        // if the target is non-existent, the face is invisible
        case (None, _) => m
        // if the target is translucent, the face is invisible
        case (Some(t), _) if t isTranslucent => m.updated(s, m(s) - v)
        // if the cover is opaque, the face is invisible
        case (_, Some(c)) if c isOpaque => m.updated(s, m(s) - v)
        // if the cover is translucent (and the target is opaque), the face is visible
        case (_, Some(c)) if c isTranslucent => m.updated(s, m(s) + v)
        // if the cover is non-existent (and the target is opaque), the face is visible
        case (_, None) => m.updated(s, m(s) + v)
        // in all other cases, the face is invisible
        case _ => m.updated(s, m(s) - v)
      }
    }
    // compute all surfaces belonging to or touching block
    def block(m: SurfaceMap, v: V3I): SurfaceMap = {
      val a: Seq[(V3I, Direction)] = Stream.iterate(v)(identity).zip(Directions())
      val b: Seq[(V3I, Direction)] = Directions().map(d => (v + d, d.neg))
      (a ++ b).foldLeft(m)({ case (m, (v, s)) => surface(m, v, s) })
    }
    // do the computation
    val empty: SurfaceMap = Directions().zip(Stream.iterate(new HashSet[V3I]())(identity)).toMap
    val blocks: Seq[V3I] = chunk.pos * chunk.size until chunk.pos + Ones * chunk.size
    val exposed: SurfaceMap = blocks.foldLeft(empty)(block)

    // create a mesh
    val mesh = new Mesh(true, 4 * 6 * chunk.blocks.length, 6 * 6 * chunk.blocks.length,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
    )

    // fold the exposure sets into vertex data and indices
    type VertDatum = (V3F, Color, V2F)
    val vertSize = mesh.getVertexSize / 4 // convert from size in bytes to size in floats
    //val p = +0.5f // positive half float
    //val n = -0.5f // negative half float
    val p = 1
    val n = 0
    val offset = chunk.pos * chunk.size

    var data: (List[VertDatum], List[Short]) = (Nil, Nil)

    def addSquareIndices(verts: List[VertDatum], indices: List[Short]): List[Short] =
      indices
        .::((verts.length + 0).toShort)
        .::((verts.length + 1).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 0).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 3).toShort)

    data = exposed(Up).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, p, p) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(p, p, p) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(p, p, n) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(n, p, n) + offset, Color.WHITE, chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(West).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, p, p) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord1))
            .::((v + V3F(n, p, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord2))
            .::((v + V3F(n, n, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord3))
            .::((v + V3F(n, n, p) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(East).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(p, p, n) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord1))
            .::((v + V3F(p, p, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord2))
            .::((v + V3F(p, n, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord3))
            .::((v + V3F(p, n, n) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(South).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord1))
            .::((v + V3F(n, p, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord2))
            .::((v + V3F(p, p, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord3))
            .::((v + V3F(p, n, n) + offset, new Color(0.85f, 0.85f, 0.85f, 1f), chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(North).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord1))
            .::((v + V3F(p, n, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord2))
            .::((v + V3F(p, p, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord3))
            .::((v + V3F(n, p, p) + offset, new Color(0.8f, 0.8f, 0.8f, 1f), chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(Down).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, p) + offset, new Color(0.75f, 0.75f, 0.75f, 1f), chunk(v).get.texCoord1))
            .::((v + V3F(n, n, n) + offset, new Color(0.75f, 0.75f, 0.75f, 1f), chunk(v).get.texCoord2))
            .::((v + V3F(p, n, n) + offset, new Color(0.75f, 0.75f, 0.75f, 1f), chunk(v).get.texCoord3))
            .::((v + V3F(p, n, p) + offset, new Color(0.75f, 0.75f, 0.75f, 1f), chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )

    val (vertices: List[VertDatum], indices: List[Short]) = data

    // reverse and serialize the vertex data into floats
    val vertexSerial = vertices.reverse.flatMap({
      case (v, color, t) => List(
        v.x, v.y, v.z,
        color toFloatBits,
        t.x, t.y
      )
    })
    // compile the vertices into an array (they were already reversed during serialization)
    val vertArr = new Array[Float](vertexSerial.size)
    var i = 0
    for (f <- vertexSerial) {
      vertArr.update(i, f)
      i += 1
    }
    // reverse and compile the indices into an array
    val indexArr = new Array[Short](indices.size)
    i = 0
    for (s <- indices.reverseIterator) {
      indexArr.update(i, s)
      i += 1
    }

    // plug the arrays into the mesh (this uploads them to VRAM)
    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    // create the material
    //val material = new Material()
    //material.set(TextureAttribute.createDiffuse(textures))
    val material = new Material
    material.set(TextureAttribute.createDiffuse(textures))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexArr.length
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable
  }

  override def apply(): Renderable = cache

}
