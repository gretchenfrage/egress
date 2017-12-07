package com.phoenixkahlo.hellcraft

import scala.reflect.ClassTag

package object fgraphics {
  type ShaderTag[S <: Shader] = ClassTag[S]
}
