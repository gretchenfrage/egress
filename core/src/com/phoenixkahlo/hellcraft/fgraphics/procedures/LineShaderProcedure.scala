package com.phoenixkahlo.hellcraft.fgraphics.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Camera, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.fgraphics._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class LineShaderProcedure extends ShaderProcedure[LineShader] {
  val program: ShaderProgram = {
    val vert = Gdx.files.internal("shaders/line_v.glsl").readString()
    val frag = Gdx.files.internal("shaders/line_f.glsl").readString()
    new ShaderProgram(vert, frag)
  }
  if (!program.isCompiled) throw new GdxRuntimeException(program.getLog)
  val u_projViewTrans = program.getUniformLocation("u_projViewTrans")
  val u_worldTrans = program.getUniformLocation("u_worldTrans")


  override def shader: ShaderTag[LineShader] = ClassTag(classOf[LineShader])

  override def toFinalForm(renderUnit: (Seq[LineShader.Vert], Seq[Short])): Mesh = {
    val (verts, indices) = renderUnit
    val floats = new ArrayBuffer[Float](verts.size * 4)
    for (vert <- verts) {
      floats.append(vert.pos.x, vert.pos.y, vert.pos.z, vert.col.toColor.toFloatBits)
    }
    val mesh = new Mesh(true, floats.size, indices.size,
      new VertexAttribute(Usage.Position, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color")
    )
    mesh.setVertices(floats.toArray)
    mesh.setIndices(indices.toArray)
    mesh
  }

  override def begin(globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.begin()
    program.setUniformMatrix(u_projViewTrans, cam.combined)

    context.setDepthTest(GL20.GL_LEQUAL)
  }

  override def apply(mesh: Mesh, params: BasicParams, globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.setUniformMatrix(u_worldTrans, params.offset.toTransMatrix)
    mesh.render(program, GL20.GL_LINES)
  }

  override def end(): Unit = {
    program.end()
  }

  override def close(): Unit = {
    program.dispose()
  }
}
