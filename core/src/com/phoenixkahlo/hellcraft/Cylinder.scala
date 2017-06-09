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

  override def update(world: World): Unit = {
    println("updating")
    // update position
    pos = pos + vel * Gdx.graphics.getDeltaTime

    // resolve collisions
    // description of how to create a resolution
    val processed = new mutable.HashSet[V3I]()

    def computeResolution(): Option[(V3I, V3F)] =
    // get the blocks with y intersection and possible horizontal intersection
      ((pos - V3F(rad, 0, rad)).floor to (pos + V3F(rad, height, rad)).floor)
        // filter out blocks that have already been processed
        .filter(!processed(_))
        // filter out non-existent or incorporeal block
        .filter(world.block(_).map(_ isCorporeal).exists(identity))
        // add horizontally-projected rectangle proximites
        .map(v => (v, RectangleProxmimity(Rectangle(v.projectHorizontal, (v + Ones).projectHorizontal), rad)))
        // filter out non-intersecting blocks
        .filter({ case (v, r) => r contains v.projectHorizontal })
        // now that we're done filtering for intersections, we convert the stream to an option
        .find(_ => true)
        // compute the best collision resolution strategy
        .map({ case (v, r) => (v, pos.closest(
          // horizontal collision resolution
          r.closestPerimiterPoint(v.projectHorizontal).horizontallyInflate(pos.y),
          // shift-up collision resolution
          V3F(pos.x, v.y + 1, pos.z),
          // shift-down collision resolution
          V3F(pos.x, v.y - height, pos.z)
        )) })

    /*
    // compute the horizontal collision resolution options
    .map({ case (v, r) => (v, r.closestPerimiterPoint(v.projectHorizontal).horizontallyInflate(pos.y)) })
    // compute the shift-up collision resolution option
    .map({ case (v, p1) => (v, p1, V3F(pos.x, v.y + 1, pos.z)) })
    // compute the shift-down collision resolution option
    .map({ case (v, p1, p2) => (v, p1, p2, V3F(pos.x, v.y - height, pos.z)) })
    // compute the updated position based on the optimal collision resolution strategy
    .map({ case (v, p1, p2, p3) => (v, List[V3F](p1, p2, p3).sortBy(_ dist pos).head) })
    */

    // create and apply resolutions
    var continue = true
    while (continue)
      computeResolution() match {
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
