package com.phoenixkahlo.hellcraft.util.audio

import com.phoenixkahlo.hellcraft.core.SoundEffect
import com.phoenixkahlo.hellcraft.fgraphics.ResourcePack
import com.phoenixkahlo.hellcraft.math.V3F

object AudioUtil {
  def linearToDecibel(linear: Float): Float =
    if (linear != 0) 20 * Math.log10(linear).toFloat
    else -144

  def decibelToLinear(decibels: Float): Float =
    Math.pow(10, decibels / 20).toFloat

  def play(pack: ResourcePack, camPos: V3F)(sound: SoundEffect): Long = {
    val dist = sound.pos dist camPos
    var decibels = linearToDecibel(sound.pow)
    if (dist > 1)
      decibels -= 6 * (Math.log(dist) / Math.log(2)).toFloat
    val volume = decibelToLinear(decibels)
    pack.sound(sound.sound).play(volume)
  }
}