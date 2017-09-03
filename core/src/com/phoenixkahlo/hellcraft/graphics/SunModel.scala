package com.phoenixkahlo.hellcraft.graphics

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, Renderable}
import com.badlogic.gdx.graphics.{GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.graphics.shaders.SunSID

class SunModel {

  val verts = Array[Float](
    -1, -1, 0,      0, 0,
    -1,  1, 0,      0, 1,
     1,  1, 0,      1, 1,
     1, -1, 0,      1, 0
  )
  val indices = Array[Short](0, 1, 2)
  /*
  val indices = Array[Short](
    0, 1, 2,
    0, 2, 3
  )
  */
  val mesh = new Mesh(true, verts.size, indices.size,
    new VertexAttribute(Usage.Position, 3, "a_position"),
    new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0")
  )
  mesh.setVertices(verts)
  mesh.setIndices(indices)
  val renderable = new Renderable
  renderable.meshPart.mesh = mesh
  renderable.meshPart.offset = 0
  renderable.meshPart.size = indices.size
  renderable.meshPart.primitiveType = GL20.GL_TRIANGLES
  renderable.material = new Material
  renderable.userData = SunSID

}
