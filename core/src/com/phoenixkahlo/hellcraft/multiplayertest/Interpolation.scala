package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.World

object Interpolation {

  def apply(world: World, continuum: ClientContinuum, clock: GametimeClock): Option[(World, Float)] =
    if (InterpolationOn) continuum.snapshot(world.time - 1) match {
        case Some(previous) => Some((previous, 1 - clock.fractionalTicksSince(previous.time)))
        case None => None
      }
    else None

}
