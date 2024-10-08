package com.phoenixkahlo.hellcraft.fgraphics.procedures

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{Camera, GL20, Mesh, VertexAttribute}
import com.badlogic.gdx.graphics.g3d.utils.RenderContext
import com.badlogic.gdx.graphics.glutils.GeomShaderProgram.ShaderPart
import com.badlogic.gdx.graphics.glutils.{GeomShaderProgram, ShaderStage}
import com.badlogic.gdx.utils.GdxRuntimeException
import com.phoenixkahlo.hellcraft.ShaderTag
import com.phoenixkahlo.hellcraft.fgraphics.{ResourcePack, _}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ParticleShaderProcedure(resources: ResourcePack) extends ShaderProcedure[ParticleShader] {
  val program: GeomShaderProgram = {
    val vert = Gdx.files.internal("shaders/particle_v.glsl").readString()
    val geom = Gdx.files.internal("shaders/particle_g.glsl").readString()
    val frag = Gdx.files.internal("shaders/particle_f.glsl").readString()
    new GeomShaderProgram(
      new ShaderPart(ShaderStage.vertex, vert),
      new ShaderPart(ShaderStage.geometry, geom),
      new ShaderPart(ShaderStage.fragment, frag)
    )
  }
  if (!program.isCompiled) throw new GdxRuntimeException(program.getLog)
  val u_MV = program.getUniformLocation("u_MV")
  val u_P = program.getUniformLocation("u_P")
  val u_texture = program.getUniformLocation("u_texture")
  val u_color = program.getUniformLocation("u_color")


  override def shader: ShaderTag[ParticleShader] = ParticleTag

  override def toFinalForm(particles: Seq[ParticleShader.Particle]): Mesh = {
    val floats = new ArrayBuffer[Float](particles.size * 9)
    for (part <- particles) {
      floats.append(
        part.pos.x, part.pos.y, part.pos.z,
        part.col.toColor.toFloatBits,
        part.size,
        part.tex0.x, part.tex0.y,
        part.tex1.x, part.tex1.y
      )
    }
    val indices = particles.indices.map(_ toShort)
    val mesh = new Mesh(true, floats.size, indices.size,
      new VertexAttribute(Usage.Generic, 3, "a_position"),
      new VertexAttribute(Usage.ColorPacked, 4, "a_color"),
      new VertexAttribute(Usage.Generic, 1, "a_size"),
      new VertexAttribute(Usage.Generic, 2, "a_texCoord0"),
      new VertexAttribute(Usage.Generic, 2, "a_texCoord1")
    )
    mesh.setVertices(floats.toArray)
    mesh.setIndices(indices.toArray)
    mesh
  }

  override def begin(globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.begin()
    resources.sheet.bind(0)
    program.setUniformi(u_texture, 0)

    context.setDepthTest(GL20.GL_LEQUAL)
    context.setCullFace(GL20.GL_BACK)
    Gdx.gl.glEnable(GL20.GL_BLEND)
    Gdx.gl.glBlendFunc(GL20.GL_SRC_ALPHA, GL20.GL_ONE_MINUS_SRC_ALPHA)
  }

  override val disposer = Some((ff: Mesh) => ff.dispose())

  override def apply(mesh: Mesh, params: ParticleShader.Params, globals: GlobalRenderData, context: RenderContext, cam: Camera): Unit = {
    program.setUniformMatrix(u_MV, params.trans.cpy().mulLeft(cam.view))
    program.setUniformMatrix(u_P, cam.projection)
    program.setUniform4fv(u_color, params.col.toArray, 0, 4)
    mesh.render(program, GL20.GL_POINTS)
  }

  override def end(): Unit = {
    program.end()
  }

  override def close(): Unit = {
    program.dispose()
  }


}
