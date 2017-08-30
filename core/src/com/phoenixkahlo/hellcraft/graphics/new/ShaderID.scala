package com.phoenixkahlo.hellcraft.graphics.`new`

sealed trait ShaderID

case object DefaultSID extends ShaderID
case object CustomSID extends ShaderID
case object LineSID extends ShaderID