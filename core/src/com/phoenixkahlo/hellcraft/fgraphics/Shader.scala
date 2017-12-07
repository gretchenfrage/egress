package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.graphics.Mesh
import com.phoenixkahlo.hellcraft.graphics.HUDComponent
import com.phoenixkahlo.hellcraft.math.{V2F, V3F, V4F}

trait Shader {
  type RenderUnit
  type Params
  type FinalForm
}

case class BasicTriVert(pos: V3F, col: V4F, tex: V2F, nor: V3F)
case class BasicParams(offset: V3F)

trait TerrainShader extends Shader {
  override type RenderUnit = (Seq[BasicTriVert], Seq[Short])
  override type Params = BasicParams
  override type FinalForm = Mesh
}

trait GenericShader extends Shader {
  override type RenderUnit = (Seq[BasicTriVert], Seq[Short])
  override type Params = BasicParams
  override type FinalForm = Mesh
}

trait LineShader extends Shader {
  override type RenderUnit = (Seq[LineShader.Vert], Seq[Short])
  override type Params = BasicParams
  override type FinalForm = Mesh
}
object LineShader {
  case class Vert(pos: V3F, col: V4F)
}

trait ParticleShader extends Shader {
  override type RenderUnit = Seq[ParticleShader.Particle]
  override type Params = BasicParams
  override type FinalForm = Mesh
}
object ParticleShader {
  case class Particle(pos: V3F, col: V4F, size: Float, tex0: V2F, tex1: V2F)
}

trait HUDShader extends Shader {
  override type RenderUnit = HUDComponent
  override type Params = Unit
  override type FinalForm = HUDComponent
}