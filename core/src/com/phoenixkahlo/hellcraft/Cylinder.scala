package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.assets.loaders.resolvers.ResolutionFileResolver.Resolution
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g3d.attributes.{ColorAttribute, FloatAttribute, TextureAttribute}
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
    new ModelInstance(builder.createCylinder(
      rad * 2, height, rad * 2, 24,
      new Material(
        TextureAttribute.createDiffuse(texture)
      ),
      Usage.Position | Usage.TextureCoordinates
    ))
  }

  var vel = V3F(0, 0, 0)

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
      .map({ case (v, r) => (v, pos.closest(
      // horizontal collision resolution
      //r.closestPerimiterPoint(v.projectHorizontal).horizontallyInflate(pos.y),
      // shift-up collision resolution
      //V3F(pos.x, v.y + 1, pos.z),
      // shift-down collision resolution
      //V3F(pos.x, v.y - height, pos.z)
    ))
    })

  override def update(world: World): Unit = {
    // update position
    pos = pos + vel * Gdx.graphics.getDeltaTime

    // resolve collisions
    val processed = new mutable.HashSet[V3I]()
    var continue = true
    while (continue)
      computeResolution(world, processed) match {
        case Some((v, p)) =>
          println("resolving collision with " + v)
          pos = p
          processed.add(v)
        case None =>
          continue = false
      }

    // update model position
    model.transform.setTranslation(pos toGdx)
  }

  override def getRenderables(renderables: Array[Renderable], pool: Pool[Renderable]): Unit = {
    model.getRenderables(renderables, pool)
  }


}
