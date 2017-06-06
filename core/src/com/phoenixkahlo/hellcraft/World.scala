package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute
import com.badlogic.gdx.graphics.{Color, GL20, Mesh, VertexAttribute, VertexAttributes}
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder
import com.badlogic.gdx.graphics.g3d.{Material, ModelInstance, Renderable, RenderableProvider}
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.utils.Pool
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable
import scala.concurrent.forkjoin.ThreadLocalRandom

class World(val size: V3I) extends RenderableProvider {

  def this(x: Int, y: Int, z: Int) =
    this(V3I(x, y, z))

  val blocks = new Array[Byte](size.xi * size.yi * size.zi)
  val exposed: Map[Direction, mutable.Set[V3I]] = Directions() map ((_, new mutable.HashSet[V3I]())) toMap
  val model = new Cache(computeModel())

  private def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def apply(v: V3I): Option[Block] =
    if (v > Origin && v < size)
      Some(BlockDirectory.lookup(blocks(compress(v))))
    else
      None

  private def computeSurface(v: V3I, surface: Direction): Unit = //TODO: return whether the surface was changed
    (this(v), this(v + surface)) match {
      case (Some(block1), Some(block2)) =>
        if (block1.isOpaque && !block2.isOpaque) {
          if (exposed(surface).add(v)) {
            model.invalidate
          }
        } else {
          if (exposed(surface).remove(v)) {
            model.invalidate
          }
        }
      case (Some(block), None) =>
        if (block isOpaque) {
          if (exposed(surface).add(v)) {
            model.invalidate
          }
        } else {
          if (exposed(surface).remove(v)) {
            model.invalidate
          }
        }
      case _ =>
    }

  def set(v: V3I, block: Block): Unit = {
    blocks.update(compress(v), block.id)
    for (direction <- Directions()) {
      computeSurface(v, direction)
      computeSurface(v + direction, direction.neg)
    }
  }

  private def computeModel(): ModelInstance = {
    println("computing model")
    val builder = new ModelBuilder()
    builder.begin()
    def makePBuilder = builder.part(
      ThreadLocalRandom.current().nextLong().toString,
      GL20.GL_TRIANGLES,
      new VertexAttributes(
        VertexAttribute.Position(),
        VertexAttribute.Normal()
      ),
      new Material(
        ColorAttribute.createDiffuse(Color.RED)
      )
    )
    for (v <- exposed(Up))
      makePBuilder.rect(
        (v + V3F(-0.5f, 0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        Up.toGdx
      )
    for (v <- exposed(Down))
      makePBuilder.rect(
        (v + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        Down.toGdx
      )
    for (v <- exposed(North))
      makePBuilder.rect(
        (v + V3F(-0.5f, -0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        North.toGdx
      )
    for (v <- exposed(South))
      makePBuilder.rect(
        (v + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        South.toGdx
      )
    for (v <- exposed(East))
      makePBuilder.rect(
        (v + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        East.toGdx
      )
    for (v <- exposed(West))
      makePBuilder.rect(
        (v + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v + V3F(-0.5f, 0.5f, -0.5f)).toGdx,
        (v + V3F(-0.5f, 0.5f, 0.5f)).toGdx,
        (v + V3F(-0.5f, -0.5f, 0.5f)).toGdx,
        West.toGdx
      )
    new ModelInstance(builder.end())
  }

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
    model().getRenderables(renderables, pool)

}
