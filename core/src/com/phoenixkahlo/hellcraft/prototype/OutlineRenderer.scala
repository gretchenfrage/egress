package com.phoenixkahlo.hellcraft.prototype

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util.{Cache, V3F, V3I}

class OutlineRenderer(var blocks: Seq[V3I], var color: Color) extends Entity {

  override def update(world: World): Unit = {}

  def buildRenderable: Renderable = {
    val mesh = new Mesh(true, 8 * blocks.length, 24 * blocks.length,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color")
    )

    type VertDatum = (V3F, Color)
    val vertSize = mesh.getVertexSize / 4
    val p = 1
    val n = 0

    var data: (List[VertDatum], List[Short]) = (Nil, Nil)

    data = blocks.foldLeft(data)(
      (data, v) => data match {
        case (verts, indices) => (
          verts
            .::((v + V3F(n, n, n), color))
            .::((v + V3F(p, n, n), color))
            .::((v + V3F(p, n, p), color))
            .::((v + V3F(n, n, p), color))
            .::((v + V3F(n, p, n), color))
            .::((v + V3F(p, p, n), color))
            .::((v + V3F(p, p, p), color))
            .::((v + V3F(n, p, p), color)),
          indices
            .::((verts.length + 0).toShort).::((verts.length + 1).toShort)
            .::((verts.length + 1).toShort).::((verts.length + 2).toShort)
            .::((verts.length + 2).toShort).::((verts.length + 3).toShort)
            .::((verts.length + 3).toShort).::((verts.length + 0).toShort)
            .::((verts.length + 4).toShort).::((verts.length + 5).toShort)
            .::((verts.length + 5).toShort).::((verts.length + 6).toShort)
            .::((verts.length + 6).toShort).::((verts.length + 7).toShort)
            .::((verts.length + 7).toShort).::((verts.length + 4).toShort)
            .::((verts.length + 0).toShort).::((verts.length + 4).toShort)
            .::((verts.length + 1).toShort).::((verts.length + 5).toShort)
            .::((verts.length + 2).toShort).::((verts.length + 6).toShort)
            .::((verts.length + 3).toShort).::((verts.length + 7).toShort)
        )
      }
    )

    val (vertices: List[VertDatum], indices: List[Short]) = data

    val vertexSerial = vertices.reverse.flatMap({
      case (v, color) => List(
        v.x, v.y, v.z,
        color toFloatBits
      )
    })

    val vertArr = new Array[Float](vertexSerial.size)
    var i = 0
    for (f <- vertexSerial) {
      vertArr.update(i, f)
      i += 1
    }

    val indexArr = new Array[Short](indices.size)
    i = 0
    for (s <- indices.reverseIterator) {
      indexArr.update(i, s)
      i += 1
    }

    mesh.setVertices(vertArr)
    mesh.setIndices(indexArr)

    val material = new Material()
    material.set(ColorAttribute.createDiffuse(color))

    val renderable = new Renderable
    renderable.meshPart.mesh = mesh
    renderable.material = material
    renderable.meshPart.offset = 0
    renderable.meshPart.size = indexArr.length
    renderable.meshPart.primitiveType = GL20.GL_LINES
    renderable
  }

  val renderableCache = new Cache(buildRenderable)

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    renderables.add(renderableCache())
  }

}
