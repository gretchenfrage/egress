package com.phoenixkahlo.hellcraft.prototype

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance, Renderable}
import com.badlogic.gdx.utils.{Array, Pool}
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

class Cylinder(
                var pos: V3F,
                val rad: Float,
                val height: Float
              ) extends Entity {

  val model: ModelInstance = {
    val texture = new Texture(Gdx.files.internal("sand.png"))
    val builder = new ModelBuilder
    val template = builder.createCylinder(
      rad * 2, height, rad * 2, 24,
      new Material(
        TextureAttribute.createDiffuse(texture)
      ),
      Usage.Position | Usage.TextureCoordinates
    )
    new ModelInstance(template)
  }

  var vel = V3F(0, 0, 0)

  var lastProcessed: Seq[V3I] = Nil

  def computeIntersecting(world: World, processed: mutable.Set[V3I]): Seq[(V3I, RectangleProxmimity)] =
  // get the blocks with y intersection and possible horizontal intersection
    ((pos - V3F(rad, 0, rad)).floor to (pos + V3F(rad, height, rad)).floor)
      // filter out blocks that have already been processed
      .filter(!processed(_))
      // filter out non-existent or incorporeal block
      .filter(world.block(_).map(_ isCorporeal).exists(identity))
      // add horizontally-projected rectangle proximites
      .map(v => (v, RectangleProxmimity(Rectangle(v.projectHorizontal, (v + Ones).projectHorizontal), rad)))
      // filter out non-intersecting blocks
      .filter({ case (_, r) => r contains pos.projectHorizontal })

  def computeResolution(world: World, processed: mutable.Set[V3I]): Option[(V3I, V3F)] =
  // find any intersecting block
    computeIntersecting(world, processed)
      .find(_ => true)
      // find the closest resolution
      .flatMap({
      case (v, r) => pos.closest(
        // horizontal collision resolution
        r.closestPerimiterPoint(pos.projectHorizontal).horizontallyInflate(pos.y),
        // shift-up collision resolution
        V3F(pos.x, v.y + 1, pos.z),
        // shift-down collision resolution
        V3F(pos.x, v.y - height, pos.z)
      ) match {
        case Some(p) => Some((v, p))
        case None => None
      }
    })

  def computeVfHorizontalCollision(vi: V2F, xi: V2F, xf: V2F): V2F = {
    val dx = xf - xi

    if (dx.magnitude == 0 || vi.magnitude == 0)
      return vi

    val vfUnit = dx.perpendicularInGeneralDirection(vi).normalize
    val vfSize = +Math.sin(Math.toRadians(dx angleWith vi)).toFloat * vi.magnitude
    vfUnit * vfSize
  }

  def applyFriction(vi: V2F, g: Float, u: Float): V2F = {
    if (vi.magnitude == 0)
      return vi
    val vf = vi - (vi.normalize * g * u * Gdx.graphics.getDeltaTime)
    if ((vi dot vf) > 0) vf
    else V2F(0, 0)
  }

  var didFriction = false

  def computeVf(vi: V3F, xi: V3F, xf: V3F, g: Float): V3F = {

    val dx = xf - xi
    if (dx.y == 0)
      computeVfHorizontalCollision(vi.projectHorizontal, xi.projectHorizontal, xf.projectHorizontal)
        .horizontallyInflate(vi.y)
    else if (!didFriction && dx.y > 0){
      didFriction = true
      applyFriction(vi.projectHorizontal, g, 0.6f).horizontallyInflate(0)
    } else V3F(vi.x, 0, vi.z)
  }


  override def update(world: World): Unit = {
    // constants
    val g = 9.8f

    // reset
    didFriction = false

    // update position
    vel = vel.copy(y = vel.y - g * Gdx.graphics.getDeltaTime)
    pos = pos + (vel * Gdx.graphics.getDeltaTime)


    // resolve collisions
    val processed = new mutable.HashSet[V3I]()
    var continue = true
    while (continue)
      computeResolution(world, processed) match {
        case Some((v, p)) =>
          vel = computeVf(vel, pos, p, g)
          //println("dx=" + (p - pos))
          pos = p
          processed.add(v)
        case None =>
          continue = false
      }
    lastProcessed = processed.toSeq

    // update model position
    model.transform.setTranslation(pos + V3F(0, height / 2, 0) toGdx)
  }

  override def getRenderables(renderables: Array[Renderable], pool: Pool[Renderable]): Unit = {
    model.getRenderables(renderables, pool)
  }


}
