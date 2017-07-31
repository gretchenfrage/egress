package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.World

import scala.collection.SortedMap

sealed trait InterpolationMode
object Off extends InterpolationMode
object Backward extends InterpolationMode
object Forward extends InterpolationMode

class Interpolator(clock: GametimeClock, mode: InterpolationMode) {

  private var worlds: SortedMap[Long, World] = SortedMap.empty

  def interpolate(current: World): (World, Option[(World, Float)]) = {
    mode match {
      case Off => (current, None)
      case Backward =>
        // manage the render history so it's the 2 most recent worlds
        worlds = worlds.updated(current.time, current)
        if (worlds.size > 2)
          worlds = worlds.drop(worlds.size - 2)
        // interpolate those worlds
        if (worlds.size == 2) (current, Some((worlds.head._2, -clock.fractionalTicksSince(worlds.firstKey))))
        else (current, None)
      case Forward =>
        // manage the render history so it's the 2 most recent worlds
        worlds = worlds.updated(current.time, current)
        if (worlds.size > 2)
          worlds = worlds.drop(worlds.size - 2)
        // interpolate those worlds
        if (worlds.size == 2) (worlds.head._2, Some((current, clock.fractionalTicksSince(worlds.firstKey) - 1)))
        else (current, None)
    }
    /*
    if (!InterpolationOn) return None
    // manage the render history so it's the 2 most recent worlds
    worlds = worlds.updated(current.time, current)
    if (worlds.size > 2)
      worlds = worlds.drop(worlds.size - 2)
    // interpolate those worlds
    if (worlds.size == 2) Some((worlds.head._2, 1 - clock.fractionalTicksSince(worlds.firstKey)))
    else None
    */
  }

}
