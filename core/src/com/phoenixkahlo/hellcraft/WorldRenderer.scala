package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Renderable, RenderableProvider}
import com.badlogic.gdx.utils
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

class WorldRenderer(val world: World) extends RenderableProvider {

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
    val mesh = new Mesh(true, 4 * 6 * world.blocks.length, 6 * 6 * world.blocks.length,
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

    // create the renderable
    val renderable = new Renderable()
    renderable.meshPart.mesh = mesh
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
  def updateSurface(v: V3I, surface: Direction): Boolean = {
    (world(v), world(v + surface)) match {
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
  /**
    * Updates the renderer to the world's current version
    */
  def update = {
    while (currentVersion < world.versionID) {
      currentVersion += 1
      val v = world.history(currentVersion)
      for (direction <- Directions()) {
        if (updateSurface(v, direction))
          renderableCache.invalidate
        if (updateSurface(v + direction, direction.neg))
          renderableCache.invalidate
      }
    }
  }


  override def getRenderables(renderables: utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    /*
    Update will read the block changes that have not been previously processed, and recompute whether the surrounding
    surfaces are visible, updating exposed as it does so, and if exposed is modified, it will invalidate the renderable
    cache, ensuring that the renderable will be freshly generated when it is next requested
     */
    update
    renderables.add(renderableCache())
  }

}
