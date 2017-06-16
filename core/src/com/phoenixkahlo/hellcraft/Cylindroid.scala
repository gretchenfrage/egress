package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance, Renderable}
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util._

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.JavaConverters

/**
  * An abstract corpus that is a vertically aligned cylinder which collides with blocks.
  */
abstract class Cylindroid[C <: Cylindroid[C]](
                override val pos: V3F,
                override val id: UUID,
                val rad: Float,
                val height: Float,
                val vel: V3F = Origin,
                val grounded: Boolean = true
                ) extends Corpus(pos, id) {

  val g = 28f

  def copy(params: (String,Any)*): C

  def naturalVelocity: V2F = V2F(0, 0)

  override protected def transform(world: World): C = {
    val u = 10f // friction
    val dt = 1f / 60f // time step

    def intersecting(c: C, exclude: Set[V3I]): Seq[(V3I, RectangleProxmimity)] =
      ((c.pos - V3F(c.rad, 0, c.rad)).floor to (c.pos + V3F(c.rad, c.height, c.rad)).floor)
      .filter(!exclude(_))
      .filter(world.blockAt(_).map(_ isCorporeal).exists(identity))
      .map(v => (v, RectangleProxmimity(
        Rectangle(
          v.flatten,
          (v + Ones).flatten
        ),
        c.rad
      )))
      .filter({ case (_, r) => r contains c.pos.flatten })

    def resolution(c: C, exclude: Set[V3I]): Option[(V3I, V3F)] =
      intersecting(c, exclude)
      .find(_ => true)
      .flatMap({ case (v, r) => c.pos.closest(
        r.closestPerimiterPoint(c.pos.flatten).inflate(c.pos.y),
        V3F(c.pos.x, v.y + 1, c.pos.z),
        V3F(c.pos.x, v.y - height, c.pos.z)
      ).map((v, _)) })

    def vfHorizontal(vi: V2F, xi: V2F, xf: V2F): V2F = {
      val dx = xf - xi
      if (dx.magnitude == 0 || vi.magnitude == 0) return vi
      val vfUnit = dx.perpendicularInGeneralDirection(vi).normalize
      val vfSize = Trig.sin(dx angleWith vi) * vi.magnitude
      vfUnit * vfSize
    }

    def vfFriction(vi: V2F): V2F = {
      val vimnv = vi - naturalVelocity
      if (vimnv.magnitude == 0) return naturalVelocity
      val reduced = vimnv - (vimnv.normalize * g * u * dt)
      if ((vimnv dot reduced) > 0) reduced + naturalVelocity
      else naturalVelocity
      /*
      if (vi.magnitude == 0) return V2F(0, 0)
      val reduced = vi - (vi.normalize * g * u * dt)
      if ((vi dot reduced) > 0) reduced
      else V2F(0, 0)
      */
    }

    case class VTransform(vf: V3F, frictioned: Boolean)
    def transformV(vi: V3F, xi: V3F, xf: V3F, friction: Boolean): VTransform = {
      val dx = xf - xi
      if (dx.y == 0)
        VTransform(
          vfHorizontal(
            vi.flatten,
            xi.flatten,
            xf.flatten
          ).inflate(vi.y)
        , false)
      else if (friction && dx.y > 0)
        VTransform(
          vfFriction(vi.flatten).inflate(0),
          true
        )
      else VTransform(V3F(vi.x, 0, vi.z), false)
    }

    def update(c: C, exclude: Set[V3I], frictioned: Boolean): C = {
      resolution(c, exclude) match {
        case Some((v, xf)) =>
          val vTransform = transformV(c.vel, c.pos, xf, !frictioned)
          val vf = vTransform.vf
          val newFrictioned = vTransform.frictioned
          val newExclude = exclude + v
          val newCylindroid = c.copy(
            "pos" -> xf,
            "vel" -> vf,
            "grounded" -> (c.grounded || newFrictioned)
          )
          update(newCylindroid, newExclude, newFrictioned)
        case None => c
      }
    }

    update(copy(
      "vel" -> vel.copy(y = vel.y - g * dt),
      "pos" -> (pos + (vel * dt)),
      "grounded" -> false
    ), new HashSet, false)
  }

  override protected def modelOffset: V3F = V3F(0, height / 2, 0)

  override protected def modelFactory(texturePack: TexturePack): ModelInstanceFactory =
    CylindroidModelFactory[C](this, texturePack)

}

case class CylindroidModelFactory[C <: Cylindroid[C]](c: Cylindroid[C], texturePack: TexturePack) extends ModelInstanceFactory {
  override def apply(): ModelInstance = {
    val builder = new ModelBuilder
    val template = builder.createCylinder(
      c.rad * 2, c.height, c.rad * 2, 24,
      new Material(
        TextureAttribute.createDiffuse(texturePack(SandTID))
      ),
      Usage.Position | Usage.TextureCoordinates
    )
    new ModelInstance(template)
  }
}