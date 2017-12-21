package com.phoenixkahlo.hellcraft.fgraphics

import com.phoenixkahlo.hellcraft.ShaderTag

class ShaderTagTable[V[_ <: Shader] <: AnyRef] {
  private val contents = new Array[AnyRef](ShaderTag.total)

  def +=[S <: Shader](v: V[S])(implicit tag: ShaderTag[S]): Unit =
    contents(tag.ordinal) = v

  def apply[S <: Shader](implicit k: ShaderTag[S]): V[S] =
    contents(k.ordinal).asInstanceOf[V[S]]

  def toSeq: Seq[V[_ <: Shader]] = contents.toSeq.asInstanceOf[Seq[V[_ <: Shader]]]
}
