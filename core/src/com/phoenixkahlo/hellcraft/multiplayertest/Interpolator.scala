package com.phoenixkahlo.hellcraft.multiplayertest

import com.phoenixkahlo.hellcraft.core.World

import scala.collection.SortedMap

sealed trait InterpolationMode
object Off extends InterpolationMode
object Backward extends InterpolationMode
object Forward extends InterpolationMode
object ForwardBounded extends InterpolationMode
object Penultimate extends InterpolationMode

class Interpolator(clock: GametimeClock, mode: InterpolationMode) {

  private var worlds: SortedMap[Long, World] = SortedMap.empty

  def interpolate(continuum: ClientContinuum): (World, Option[(World, Float)]) = {
    lazy val current = continuum.current
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
      case ForwardBounded =>
        // manage the render history so it's the 2 most recent worlds
        worlds = worlds.updated(current.time, current)
        if (worlds.size > 2)
          worlds = worlds.drop(worlds.size - 2)
        // interpolate those worlds
        if (worlds.size == 2) (worlds.head._2, Some((current,
          Math.min(Math.max(clock.fractionalTicksSince(worlds.firstKey) - 1, 0), 1))))
        else (current, None)
      case Penultimate =>
        val t = clock.gametime - 1
        val f = clock.fractionalTicksSince(t) - 1
        (continuum.snapshot(t), continuum.snapshot(t + 1)) match {
          case (Some(old), Some(neu)) => (old, Some((neu, f)))
          case _ => (continuum.current, None)
        }
    }
  }

}
