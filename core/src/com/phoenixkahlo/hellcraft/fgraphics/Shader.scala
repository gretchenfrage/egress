package com.phoenixkahlo.hellcraft.fgraphics

import com.badlogic.gdx.graphics.Mesh
import com.badlogic.gdx.math.Matrix4
import com.phoenixkahlo.hellcraft.graphics.HUDComponent
import com.phoenixkahlo.hellcraft.math._

trait Shader {
  type RenderUnit
  type Params
  type FinalForm
}

case class BasicTriVert(pos: V3F, col: V4F, tex: V2F, nor: V3F)
case class Offset(offset: V3F)
case class TransMatrix(mat: Matrix4)
object Offset {
  val default = Offset(Origin)
}

trait TerrainShader extends Shader {
  override type RenderUnit = (Seq[BasicTriVert], Seq[Short])
  override type Params = Offset
  override type FinalForm = Mesh
}

trait GenericShader extends Shader {
  override type RenderUnit = (Seq[BasicTriVert], Seq[Short])
  override type Params = Offset
  override type FinalForm = Mesh
}

trait LineShader extends Shader {
  override type RenderUnit = (Seq[LineShader.Vert], Seq[Short])
  override type Params = Offset
  override type FinalForm = Mesh
}
object LineShader {
  case class Vert(pos: V3F, col: V4F)
}

trait ParticleShader extends Shader {
  override type RenderUnit = Seq[ParticleShader.Particle]
  override type Params = ParticleShader.Params
  override type FinalForm = Mesh
}
object ParticleShader {
  case class Particle(pos: V3F, col: V4F, size: Float, tex0: V2F, tex1: V2F)
  case class Params(trans: Matrix4, col: V4F = V4I.ones)
}

trait HUDShader extends Shader {
  override type RenderUnit = HUDComponent
  override type Params = Unit
  override type FinalForm = HUDComponent
}