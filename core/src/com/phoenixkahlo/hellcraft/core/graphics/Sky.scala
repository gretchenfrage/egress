package com.phoenixkahlo.hellcraft.core.graphics

import com.phoenixkahlo.hellcraft.core.request.ExecSeq
import com.phoenixkahlo.hellcraft.fgraphics.ParticleShader.Particle
import com.phoenixkahlo.hellcraft.fgraphics.{GEval, ParticleShader, Renderable}
import com.phoenixkahlo.hellcraft.graphics.{MoonTID, StarTID, SunTID}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc

import scala.util.Random

case class SkyParams(skyDist: Float)
object SunMoon extends MemoFunc[SkyParams, Renderable[ParticleShader]](input => {
  implicit val exec = ExecSeq
  Renderable[ParticleShader](GEval.resourcePack.map(pack => {
    Seq(
      Particle(
        East * input.skyDist, V4F(1, 1, 1, 1), input.skyDist / 8f,
        V2F(pack(SunTID).getU, pack(SunTID).getV),
        V2F(pack(SunTID).getU2, pack(SunTID).getV2)
      ),
      Particle(
        West * input.skyDist, V4F(1, 1, 1, 1), input.skyDist / 8f,
        V2F(pack(MoonTID).getU, pack(SunTID).getV),
        V2F(pack(MoonTID).getU2, pack(MoonTID).getV2)
      )
    )
  }))
})
object Stars extends MemoFunc[SkyParams, Renderable[ParticleShader]](input => {
  implicit val exec = ExecSeq
  Renderable[ParticleShader](GEval.resourcePack.map(pack => {
    val random = new Random(9287364589374265L)
    (1 to 1000).map(_ => {
      val t = random.nextFloat() * 360
      val z = random.nextFloat() * 2 - 1
      val dir = V3F(
        Math.sqrt(1 - z * z).toFloat * Trig.cos(t),
        Math.sqrt(1 - z * z).toFloat * Trig.sin(t),
        z
      )
      val pow = random.nextFloat()
      val size = random.nextFloat() + 1
      Particle(
        dir * input.skyDist, V4F(1, 1, 1, pow), input.skyDist / 128f / size,
        V2F(pack(StarTID).getU, pack(StarTID).getV),
        V2F(pack(StarTID).getU2, pack(StarTID).getV2)
      )
    })
  }))
})
/*
object Sky extends MemoFunc[SkyParams, Renderable[ParticleShader]](input => {
  implicit val exec = ExecSeq
  val eval = GEval.resourcePack.map(pack => {
    val sun = Particle(
      East * input.skyDist, V4F(1, 1, 1, 1), input.skyDist / 8f,
      V2F(pack(SunTID).getU, pack(SunTID).getV),
      V2F(pack(SunTID).getU2, pack(SunTID).getV2)
    )
    val moon = Particle(
      West * input.skyDist, V4F(1, 1, 1, 1), input.skyDist / 8f,
      V2F(pack(MoonTID).getU, pack(SunTID).getV),
      V2F(pack(MoonTID).getU2, pack(MoonTID).getV2)
    )
    val random = new Random(9287364589374265L)
    val stars = (1 to 1000).map(_ => {
      val t = random.nextFloat() * 360
      val z = random.nextFloat() * 2 - 1
      val dir = V3F(
        Math.sqrt(1 - z * z).toFloat * Trig.cos(t),
        Math.sqrt(1 - z * z).toFloat * Trig.sin(t),
        z
      )
      val pow = random.nextFloat()
      val size = random.nextFloat() + 1
      Particle(
        dir * input.skyDist, V4F(1, 1, 1, pow), input.skyDist / 128f / size,
        V2F(pack(StarTID).getU, pack(StarTID).getV),
        V2F(pack(StarTID).getU2, pack(StarTID).getV2)
      )
    })
    stars :+ sun :+ moon
  })
  Renderable[ParticleShader](eval)
})
*/