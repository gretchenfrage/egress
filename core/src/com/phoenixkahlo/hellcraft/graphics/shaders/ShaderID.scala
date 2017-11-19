package com.phoenixkahlo.hellcraft.graphics.shaders

import com.badlogic.gdx.graphics.VertexAttribute

sealed class ShaderID(val primType: Int, val attribs: VertexAttribute*)
case object TerrainSID extends ShaderID(0)
case object LineSID extends ShaderID(0)
case object PointSID extends ShaderID(0)
case object BasicSID extends ShaderID(0)
case object GenericSID extends ShaderID(0)
case object ParticleSID extends ShaderID(0)