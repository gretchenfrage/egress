package com.phoenixkahlo.hellcraft


import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.phoenixkahlo.hellcraft.util._

import scala.concurrent.duration._
import scala.collection.immutable.{HashMap, HashSet}
import scala.concurrent.{Await, ExecutionContext, Future}

case class ChunkRenderer(chunk: Chunk, texturePack: TexturePack, world: World) extends RenderableFactory {

  val meshData: Future[(Array[Float], Array[Short])] = Future {
    // first, compute the exposed surfaces
    type SurfaceMap = Map[Direction, List[V3I]]

    // compute the given surface of the given block
    def surface(m: SurfaceMap, v: V3I, s: Direction): SurfaceMap =
      (world.blockAt(v), world.blockAt(v + s)) match {
        // if the target is non-existent, the face is invisible
        case (None, _) => m
        // if the target is translucent, the face is invisible
        case (Some(t), _) if t isTranslucent => m
        // if the cover is opaque, the face is invisible
        case (_, Some(c)) if c isOpaque => m
        // if the cover is translucent (and the target is opaque), the face is visible
        case (_, Some(c)) if c isTranslucent => m.updated(s, v :: m(s))
        // if the cover is non-existent (and the target is opaque), the face is visible
        case (_, None) => m.updated(s, v :: m(s))
        // in all other cases, the face is invisible
        case _ => m
      }

    // compute all surfaces of a block
    def block(m: SurfaceMap, v: V3I): SurfaceMap =
      (Stream.iterate(v)(identity) zip Directions()).foldLeft(m)({ case (m, (v, s)) => surface(m, v, s) })

    // do the computation
    val empty: SurfaceMap = Directions() zip Stream.iterate(Nil)(identity) toMap
    val blocks: Seq[V3I] = (Origin until V3I(chunk.size, chunk.size, chunk.size)) map (_ + (chunk.pos * chunk.size))
    val exposed: SurfaceMap = blocks.foldLeft(empty)(block) //.mapValues(_.map(_ - (chunk.pos * chunk.size)))

    // fold the exposure sets into vertex data and indices
    type VertDatum = (V3F, Color, V2F)
    val vertSize = 6
    // convert from size in bytes to size in floats
    val p = 1
    val n = 0
    val offset = chunk.pos * chunk.size

    def addSquareIndices(verts: List[VertDatum], indices: List[Short]): List[Short] =
      indices
        .::((verts.length + 0).toShort)
        .::((verts.length + 1).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 0).toShort)
        .::((verts.length + 2).toShort)
        .::((verts.length + 3).toShort)

    var data: (List[VertDatum], List[Short]) = (Nil, Nil)

    data = exposed(Up).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, p, p), Color.WHITE, V2F(r.getU, r.getV)))
              .::((b + V3F(p, p, p), Color.WHITE, V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, n), Color.WHITE, V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, p, n), Color.WHITE, V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(West).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, p, p), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(n, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, n, p), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(East).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(p, p, n), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(p, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, n), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(South).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, n), new Color(0.85f, 0.85f, 0.85f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(North).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(p, n, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(n, p, p), new Color(0.8f, 0.8f, 0.8f, 1f), V2F(r.getU, r.getV2))),
            addSquareIndices(verts, indices)
          )
      }
    )
    data = exposed(Down).foldLeft(data)(
      (data, b) => data match {
        case (verts, indices) =>
          val r = texturePack(world.blockAt(b).get.tid)
          (
            verts
              .::((b + V3F(n, n, p), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU, r.getV)))
              .::((b + V3F(n, n, n), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU2, r.getV)))
              .::((b + V3F(p, n, n), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU2, r.getV2)))
              .::((b + V3F(p, n, p), new Color(0.75f, 0.75f, 0.75f, 1f), V2F(r.getU, r.getV2))),
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

    (vertArr, indexArr)
  }(ExecutionContext.global)

  lazy val renderable: Renderable = {
    // create a mesh
    val mesh = new Mesh(true, 4 * 6 * chunk.blocks.length, 6 * 6 * chunk.blocks.length,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
    )

    // get the arrays
    val (vertArr, indexArr) = Await.result(meshData, Duration.Inf)

    // plug the arrays into the mesh (this uploads them to VRAM)
    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    // create the material
    val material = new Material
    material.set(TextureAttribute.createDiffuse(texturePack.texture))

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexArr.length
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable
  }

  override def apply(): Seq[Renderable] =
    if (meshData.isCompleted) Seq(renderable)
    else Nil

}
