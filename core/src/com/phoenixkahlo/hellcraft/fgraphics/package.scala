package com.phoenixkahlo.hellcraft

import com.phoenixkahlo.hellcraft.fgraphics.{GenericTag, HUDTag, LineTag, ParticleTag, Shader, TerrainTag}

import scala.reflect.ClassTag

case class ShaderTag[S <: Shader](ordinal: Int)
object ShaderTag {
  val total = 5
  val tags: Seq[ShaderTag[_ <: Shader]] =
    Seq(GenericTag, HUDTag, LineTag, ParticleTag, TerrainTag)
}

package object fgraphics {
  //type ShaderTag[S <: Shader] = ClassTag[S]

  implicit object GenericTag extends ShaderTag[GenericShader](0)
  implicit object HUDTag extends ShaderTag[HUDShader](1)
  implicit object LineTag extends ShaderTag[LineShader](2)
  implicit object ParticleTag extends ShaderTag[ParticleShader](3)
  implicit object TerrainTag extends ShaderTag[TerrainShader](4)


}
