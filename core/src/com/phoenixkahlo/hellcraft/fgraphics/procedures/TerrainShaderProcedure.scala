package com.phoenixkahlo.hellcraft.fgraphics.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderStage}
import com.badlogic.gdx.graphics._
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.fgraphics.{ResourcePack, _}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class TerrainShaderProcedure(resources: ResourcePack) extends ShaderProcedure[TerrainShader] {
  val program: GeomShaderProgram = {
    val vert = Gdx.files.internal("shaders/terrain_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/terrain_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/terrain_f.glsl").readString()
    new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )
  }
  if (!program.isCompiled) throw new GdxRuntimeException(program.getLog)
  val u_worldTrans = program.getUniformLocation("u_worldTrans")
  val u_viewTrans = program.getUniformLocation("u_viewTrans")
  val u_projTrans = program.getUniformLocation("u_projTrans")
  val u_lightPos = program.getUniformLocation("u_lightPos")
  val u_texture = program.getUniformLocation("u_texture")
  val u_lightPow = program.getUniformLocation("u_lightPow")


  override def shader: ShaderTag[TerrainShader] = ClassTag(classOf[TerrainShader])

  override def toFinalForm(renderUnit: (Seq[BasicTriVert], Seq[Short])): Mesh = {
    val (verts, indices) = renderUnit
    val floats = new ArrayBuffer[Float](verts.size * 9)
    for (vert <- verts) {
      floats.append(
        vert.pos.x, vert.pos.y, vert.pos.z,
        Color.WHITE.toFloatBits,
        vert.tex.x, vert.tex.y,
        vert.nor.x, vert.nor.y, vert.nor.z
      )
    }
    val mesh = new Mesh(true, floats.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.TextureCoordinates, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Normal, 3, "a_normal")
    )
    mesh.setVertices(floats.toArray)
    mesh.setIndices(indices.toArray)
    mesh
  }

  override def begin(globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.begin()
    program.setUniformMatrix(u_viewTrans, cam.view)
    program.setUniformMatrix(u_projTrans, cam.projection)
    program.setUniform3fv(u_lightPos, globals.lightPos.toArray, 0, 3)
    resources.sheet.bind(0)
    program.setUniformi(u_texture, 0)
    program.setUniformf(u_lightPow, globals.lightPow)

    context.setDepthTest(GL20.GL_LEQUAL)
    context.setCullFace(GL20.GL_BACK)
  }

  override def apply(mesh: Mesh, params: Offset, globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.setUniformMatrix(u_worldTrans, params.offset.toTransMatrix)
    mesh.render(program, GL20.GL_TRIANGLES)
  }

  override def end(): Unit = {
    program.end()
  }

  override def close(): Unit = {
    program.dispose()
  }
}
