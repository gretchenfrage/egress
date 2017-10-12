package com.phoenixkahlo.hellcraft.graphics.models

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.g3d.model.MeshPart
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, Model}
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute}
import com.phoenixkahlo.hellcraft.util.caches.KeyParamPool

/**
  * Created by kahlo on 8/21/2017.
  */
object BlockOutlineModel extends KeyParamPool[Color,Color,Model](color => {
  val n = 0 - 1e-3f
  val p = 1 + 1e-3f
  val verts: Array[Float] = Array[Float](
    n, n, n, color toFloatBits,
    p, n, n, color toFloatBits,
    p, n, p, color toFloatBits,
    n, n, p, color toFloatBits,
    n, p, n, color toFloatBits,
    p, p, n, color toFloatBits,
    p, p, p, color toFloatBits,
    n, p, p, color toFloatBits
  )
  val indices: Array[Short] = Array[Short](
    0, 1,
    1, 2,
    2, 3,
    3, 0,
    4, 5,
    5, 6,
    6, 7,
    7, 4,
    0, 4,
    1, 5,
    2, 6,
    3, 7
  )

  val mesh = new Mesh(true, 8, 24,
    new VertexAttribute(Usage.Position, 3, "a_position"),
    new VertexAttribute(Usage.ColorPacked, 4, "a_color")
  )

  mesh.setVertices(verts)
  mesh.setIndices(indices)

  val material = new Material()
  material.set(ColorAttribute.createDiffuse(color))

  val meshPart = new MeshPart("outline", mesh, 0, mesh.getNumIndices, GL20.GL_LINES)

  val builder = new ModelBuilder()
  builder.begin()
  builder.part(meshPart, material)
  builder.end()

})
