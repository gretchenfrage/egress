package com.phoenixkahlo.hellcraft.graphics.`new`

import com.phoenixkahlo.hellcraft.core.World

sealed trait Interpolation

object NoInterpolation extends Interpolation

case class InterpolateWith(world: World, fraction: Float)