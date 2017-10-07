package com.phoenixkahlo.hellcraft.graphics


import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, Renderable, RenderableProvider}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.utils
import com.badlogic.gdx.utils.{Disposable, Pool}
import com.phoenixkahlo.hellcraft.graphics.shaders.{TerrainSID}

class SunModel extends RenderableProvider with Disposable {

  val verts = Array[Float](
    -1, -1, 0,    Color.WHITE.toFloatBits,    0, 0,    0, 0, -1,
    -1,  1, 0,    Color.WHITE.toFloatBits,    0, 1,    0, 0, -1,
     1,  1, 0,    Color.WHITE.toFloatBits,    1, 1,    0, 0, -1,
     1, -1, 0,    Color.WHITE.toFloatBits,    1, 0,    0, 0, -1
  )
  val indices = Array[Short](
    0, 1, 2,
    0, 2, 3
  )
  val mesh = new Mesh(true, verts.length, indices.length,
    new VertexAttribute(Usage.Position, 3, "a_position"),
    new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
    new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
    new VertexAttribute(Usage.Normal, 3, "a_normal")
  )
  mesh.setVertices(verts)
  mesh.setIndices(indices)

  val renderable = new Renderable()
  renderable.meshPart.mesh = mesh
  renderable.material = new Material
  renderable.meshPart.offset = 0
  renderable.meshPart.size = indices.size
  renderable.meshPart.primitiveType = GL20.GL_TRIANGLES

  override def getRenderables(renderables: utils.Array[Renderable], pool: Pool[Renderable]): Unit = {
    renderables.add(renderable)
  }

  override def dispose(): Unit = {
    mesh.dispose()
  }

}
