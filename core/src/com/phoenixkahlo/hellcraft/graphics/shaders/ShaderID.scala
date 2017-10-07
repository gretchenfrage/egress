package com.phoenixkahlo.hellcraft.graphics.shaders

sealed trait ShaderID
case object SceneSID extends ShaderID
case object LineSID extends ShaderID
case object PointSID extends ShaderID
case object BasicSID extends ShaderID
case object GenericSID extends ShaderID