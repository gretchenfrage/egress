package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute, VertexAttributes}
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance, Renderable, RenderableProvider}
import com.badlogic.gdx.math.{MathUtils, Vector3}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable
import scala.concurrent.forkjoin.ThreadLocalRandom

class World(val size: V3I) extends RenderableProvider {

  def this(x: Int, y: Int, z: Int) =
    this(V3I(x, y, z))

  val blocks = new Array[Byte](size.monoidFold(_ * _))
  val exposed: Map[Direction, mutable.Set[V3I]] = Directions() map ((_, new mutable.HashSet[V3I]())) toMap

  val renderable = new Cache[Renderable]({
    // create a mesh
    val mesh = new Mesh(true, 4 * 6 * blocks.length, 6 * 6 * blocks.length,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"))
    val vertSize = mesh.getVertexSize / 4 // convert from size in bytes to size in floats

    // fold the exposure sets into vertex data and indices
    type VertDatum = (V3F, Color)
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
            .::((v + V3F(-0.5f, 0.5f, 0.5f), Color.RED))
            .::((v + V3F(0.5f, 0.5f, 0.5f), Color.RED))
            .::((v + V3F(0.5f, 0.5f, -0.5f), Color.RED))
            .::((v + V3F(-0.5f, 0.5f, -0.5f), Color.RED)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(West).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(-0.5f, 0.5f, 0.5f), Color.PURPLE))
            .::((v + V3F(-0.5f, 0.5f, -0.5f), Color.PURPLE))
            .::((v + V3F(-0.5f, -0.5f, -0.5f), Color.PURPLE))
            .::((v + V3F(-0.5f, -0.5f, 0.5f), Color.PURPLE)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(East).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(0.5f, 0.5f, -0.5f), Color.YELLOW))
            .::((v + V3F(0.5f, 0.5f, 0.5f), Color.YELLOW))
            .::((v + V3F(0.5f, -0.5f, 0.5f), Color.YELLOW))
            .::((v + V3F(0.5f, -0.5f, -0.5f), Color.YELLOW)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(South).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(-0.5f, -0.5f, -0.5f), Color.ORANGE))
            .::((v + V3F(-0.5f, 0.5f, -0.5f), Color.ORANGE))
            .::((v + V3F(0.5f, 0.5f, -0.5f), Color.ORANGE))
            .::((v + V3F(0.5f, -0.5f, -0.5f), Color.ORANGE)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(North).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(-0.5f, -0.5f, 0.5f), Color.BLUE))
            .::((v + V3F(0.5f, -0.5f, 0.5f), Color.BLUE))
            .::((v + V3F(0.5f, 0.5f, 0.5f), Color.BLUE))
            .::((v + V3F(-0.5f, 0.5f, 0.5f), Color.BLUE)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(Down).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(-0.5f, -0.5f, 0.5f), Color.GREEN))
            .::((v + V3F(-0.5f, -0.5f, -0.5f), Color.GREEN))
            .::((v + V3F(0.5f, -0.5f, -0.5f), Color.GREEN))
            .::((v + V3F(0.5f, -0.5f, 0.5f), Color.GREEN)),
          addSquareIndices(verts, indices)
        )
      }
    )

    // compile the vertex data into an array
    val (vertices: List[VertDatum], indices: List[Short]) = data

    val vertArr = new Array[Float](vertices.size * vertSize)
    var i = 0
    for (vertDatum <- vertices.reverseIterator) {
      vertDatum match {
        case (v, color) =>
          vertArr.update(i + 0, v.x)
          vertArr.update(i + 1, v.y)
          vertArr.update(i + 2, v.z)
          vertArr.update(i + 3, color toFloatBits)
          i += vertSize
      }
    }
    // compile the indices into an array
    val indexArr = new Array[Short](indices.size)
    i = 0
    for (f <- indices.reverseIterator) {
      indexArr.update(i, f)
      i += 1
    }

    // plug the arrays into the mesh (this uploads them to VRAM)
    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    /*
    val u = Color.toFloatBits(255, 0, 0, 255)
    var l = Color.toFloatBits(0, 255, 0, 255)
    val r = Color.toFloatBits(0, 0, 255, 255)
    val s = Color.ORANGE.toFloatBits
    val n = Color.YELLOW.toFloatBits
    val d = Color.PURPLE.toFloatBits
    mesh.setVertices(Array[Float](
      -0.5f, 0.5f, 0.5f, u,
      0.5f, 0.5f, 0.5f, u,
      0.5f, 0.5f, -0.5f, u,
      -0.5f, 0.5f, -0.5f, u,

      -0.5f, 0.5f, 0.5f, l,
      -0.5f, 0.5f, -0.5f, l,
      -0.5f, -0.5f, -0.5f, l,
      -0.5f, -0.5f, 0.5f, l,

      0.5f, 0.5f, -0.5f, r,
      0.5f, 0.5f, 0.5f, r,
      0.5f, -0.5f, 0.5f, r,
      0.5f, -0.5f, -0.5f, r,

      -0.5f, -0.5f, -0.5f, s,
      -0.5f, 0.5f, -0.5f, s,
      0.5f, 0.5f, -0.5f, s,
      0.5f, -0.5f, -0.5f, s,

      -0.5f, -0.5f, 0.5f, n,
      0.5f, -0.5f, 0.5f, n,
      0.5f, 0.5f, 0.5f, n,
      -0.5f, 0.5f, 0.5f, n,

      -0.5f, -0.5f, 0.5f, d,
      -0.5f, -0.5f, -0.5f, d,
      0.5f, -0.5f, -0.5f, d,
      0.5f, -0.5f, 0.5f, d
    ))
    mesh.setIndices(Array[Short](
      0, 1, 2,
      0, 2, 3,

      4, 5, 6,
      4, 6, 7,

      8, 9, 10,
      8, 10, 11,

      12, 13, 14,
      12, 14, 15,

      16, 17, 18,
      16, 18, 19,

      20, 21, 22,
      20, 22, 23
    ))
    */

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexArr.length
    renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
    renderable
  })
  /*
  val mesh = new Mesh(
    true,
    size.xi * size.yi * size.zi * 12 * 3,
    size.xi * size.yi * size.zi * 12 * 3,
    VertexAttribute.Position(),
    VertexAttribute.Normal()
  )
  val material = new Material(
    new ColorAttribute(
      ColorAttribute.Diffuse,
      MathUtils.random(0.5f, 1f),
      MathUtils.random(0.5f, 1f),
      MathUtils.random(0.5f, 1f), 1
    ))
  val renderable = new Cache[Renderable]({
    // create mesh
    val upVecs = exposed(Up).flatMap(v =>
      Array(
        v + V3F(-0.5f, 0.5f, 0.5f),
        V3I(0, 1, 0),
        v + V3F(0.5f, 0.5f, 0.5f),
        V3I(0, 1, 0),
        v + V3F(0.5f, 0.5f, -0.5f),
        V3I(0, 1, 0),
        v + V3F(-0.5f, 0.5f, -0.5f),
        V3I(0, 1, 0)
      )
    )
    val downVecs = exposed(Down).flatMap(v =>
      Array(
        v + V3F(-0.5f, -0.5f, -0.5f),
        V3I(0, -1, 0),
        v + V3F(0.5f, -0.5f, -0.5f),
        V3I(0, -1, 0),
        v + V3F(0.5f, -0.5f, 0.5f),
        V3I(0, -1, 0),
        v + V3F(-0.5f, -0.5f, 0.5f),
        V3I(0, -1, 0)
      )
    )
    val northVecs = exposed(North).flatMap(v =>
      Array(
        v + V3F(-0.5f, -0.5f, 0.5f),
        V3I(0, 0, -1),
        v + V3F(0.5f, -0.5f, 0.5f),
        V3I(0, 0, -1),
        v + V3F(0.5f, 0.5f, 0.5f),
        V3I(0, 0, -1),
        v + V3F(-0.5f, 0.5f, 0.5f),
        V3I(0, 0, -1)
      )
    )
    val southVecs = exposed(South).flatMap(v =>
      Array(
        v + V3F(-0.5f, 0.5f, -0.5f),
        V3I(0, 0, 1),
        v + V3F(0.5f, 0.5f, -0.5f),
        V3I(0, 0, 1),
        v + V3F(0.5f, -0.5f, -0.5f),
        V3I(0, 0, 1),
        v + V3F(-0.5f, -0.5f, -0.5f),
        V3I(0, 0, 1)
      )
    )
    val eastVecs = exposed(East).flatMap(v =>
      Array(
        v + V3F(0.5f, -0.5f, -0.5f),
        V3I(1, 0, 0),
        v + V3F(0.5f, 0.5f, -0.5f),
        V3I(1, 0, 0),
        v + V3F(0.5f, 0.5f, 0.5f),
        V3I(1, 0, 0),
        v + V3F(0.5f, -0.5f, 0.5f),
        V3I(1, 0, 0)
      )
    )
    val westVecs = exposed(West).flatMap(v =>
      Array(
        v + V3F(-0.5f, -0.5f, 0.5f),
        V3I(-1, 0, 0),
        v + V3F(-0.5f, 0.5f, 0.5f),
        V3I(-1, 0, 0),
        v + V3F(-0.5f, 0.5f, -0.5f),
        V3I(-1, 0, 0),
        v + V3F(-0.5f, -0.5f, -0.5f),
        V3I(-1, 0, 0)
      )
    )
    val vecs = List(upVecs, downVecs, northVecs, southVecs, eastVecs, westVecs).flatten
    val floats = vecs.flatMap(_.floatSeq)

    val vertArr = new Array[Float](floats.size)
    var i = 0
    for (f <- floats) {
      vertArr.update(i, f)
      i = i + 1
    }

    val indices = 0.until(vecs.size / 2).map(_.toShort)
    val indexArr = new Array[Short](indices.size)
    i = 0
    for (s <- indices) {
      indexArr.update(i, s)
      i += 1
    }

    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    // create renderable
    val r = new Renderable()
    r.material = material
    r.meshPart.mesh = mesh
    r.meshPart.offset = 0
    r.meshPart.size = vecs.size / 2
    r.meshPart.primitiveType = GL20.GL_TRIANGLES
    r
  })
  */

  private def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < size)
      Some(BlockDirectory.lookup(blocks(compress(v))))
    else
      None

  def computeSurface(v: V3I, surface: Direction): Boolean = {
    (this(v), this(v + surface)) match {
        // if the target is non-existent, the face is non-existent (invisible)
      case (None, _) => false
        // if the target is translucent, the face is invisible
      case (Some(target), _) if target isTranslucent => exposed(surface).remove(v)
        // if the cover is opaque, the face is invisible
      case (_, Some(cover)) if cover isOpaque => exposed(surface).remove(v)
        // if the cover is translucent (and the target is opaque), the face is visible
      case (_, Some(cover)) if cover isTranslucent => exposed(surface).add(v)
        // if the cover is non-existent (and the target is opaque), the face is visible
      case (_, None) => exposed(surface).add(v)
        // in all other cases, the face is invisible
      case _ => exposed(surface).remove(v)
    }
  }

  def set(v: V3I, block: Block): Unit = {
    blocks.update(compress(v), block.id)
    for (direction <- Directions()) {
      if (computeSurface(v, direction))
        renderable.invalidate
      if (computeSurface(v + direction, direction.neg))
        renderable.invalidate
    }
  }

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    renderables.add(renderable())
  }


}
