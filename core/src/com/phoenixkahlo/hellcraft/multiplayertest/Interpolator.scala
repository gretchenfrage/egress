package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.World

import scala.collection.SortedMap

class Interpolator(clock: GametimeClock) {

  private var worlds: SortedMap[Long, World] = SortedMap.empty

  def interpolate(current: World): Option[(World, Float)] = {
    if (!InterpolationOn) None
    // manage the render history so it's the 2 most recent worlds
    worlds = worlds.updated(current.time, current)
    if (worlds.size > 2)
      worlds = worlds.drop(worlds.size - 2)
    // interpolate those worlds
    if (worlds.size == 2) Some((worlds.head._2, 1 - clock.fractionalTicksSince(worlds.firstKey)))
    else None
  }

}
