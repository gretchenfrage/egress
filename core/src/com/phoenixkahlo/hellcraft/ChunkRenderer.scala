package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics._
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.g3d.{Material, Renderable, RenderableProvider}
import com.badlogic.gdx.{Gdx, utils}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

class ChunkRenderer(
                     val chunk: Chunk,
                     val offset: V3F,
                     val textures: Texture
                   ) extends RenderableProvider {

  /**
    * The world's versionID that this renderer is currently updated to
    */
  var currentVersion: Long = 0
  /**
    * Collection of all visible surfaces
    */
  val exposed: Map[Direction, mutable.Set[V3I]] = Directions() map ((_, new mutable.HashSet[V3I]())) toMap
  /**
    * Builds a renderable of all visible surfaces based on the exposed structure
    */
  def buildRenderable: Renderable = {
    // create a mesh
    val mesh = new Mesh(true, 4 * 6 * chunk.blocks.length, 6 * 6 * chunk.blocks.length,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
    )

    // fold the exposure sets into vertex data and indices
    type VertDatum = (V3F, Color, V2F)
    val vertSize = mesh.getVertexSize / 4 // convert from size in bytes to size in floats
    val p = +0.5f // positive half float
    val n = -0.5f // negative half float

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
            .::((v + V3F(n, p, p) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(n, p, n) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(n, n, n) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(n, n, p) + offset, Color.WHITE, chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(East).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(p, p, n) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(p, p, p) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(p, n, p) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(p, n, n) + offset, Color.WHITE, chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(South).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, n) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(n, p, n) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(p, p, n) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(p, n, n) + offset, Color.WHITE, chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(North).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, p) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(p, n, p) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(p, p, p) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(n, p, p) + offset, Color.WHITE, chunk(v).get.texCoord4)),
          addSquareIndices(verts, indices)
        )
      }
    )
    data = exposed(Down).foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, p) + offset, Color.WHITE, chunk(v).get.texCoord1))
            .::((v + V3F(n, n, n) + offset, Color.WHITE, chunk(v).get.texCoord2))
            .::((v + V3F(p, n, n) + offset, Color.WHITE, chunk(v).get.texCoord3))
            .::((v + V3F(p, n, p) + offset, Color.WHITE, chunk(v).get.texCoord4)),
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
    val material = new Material()
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
  /**
    * Cache of the renderable
    */
  val renderableCache = new Cache(buildRenderable)
  /**
    * Computes whether the surface is visible, and updates exposed, returning whether anything was changed.
    */
  def updateSurface(v: V3I, surface: Direction): Boolean =
    (chunk(v), chunk(v + surface)) match {
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
  /**
    * Updates the renderer to the world's current version
    */
  def update(): Unit =
    while (currentVersion < chunk.versionID) {
      currentVersion += 1
      val v = chunk.history(currentVersion)
      for (direction <- Directions()) {
        if (updateSurface(v, direction))
          renderableCache.invalidate
        if (updateSurface(v + direction, direction.neg))
          renderableCache.invalidate
      }
    }


  override def getRenderables(renderables: utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    /*
    Update will read the block changes that have not been previously processed, and recompute whether the surrounding
    surfaces are visible, updating exposed as it does so, and if exposed is modified, it will invalidate the renderable
    cache, ensuring that the renderable will be freshly generated when it is next requested
     */
    update()
    renderables.add(renderableCache())
  }

}
