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

class World(val xs: Int, val ys: Int, val zs: Int) extends RenderableProvider {

  val blocks = new Array[Byte](xs * ys * zs)
  val exposed: Map[Direction, mutable.Set[V3I]] = Directions() map ((_, new mutable.HashSet[V3I]())) toMap
  val model = new Cache(computeModel())

  private def compress(v: V3I): Int =
    v.x + v.z * xs + v.y * xs * zs

  def apply(v: V3I): Block =
    BlockDirectory.lookup(blocks(compress(v)))

  private def computeSurface(v: V3I, surface: Direction): Unit = {
    if (this(v).isOpaque || this(v + surface).isOpaque)
      exposed(surface).add(v)
    else
      exposed(surface).remove(v)
  }

  def set(v: V3I, block: Block): Unit = {
    blocks.update(compress(v), block.id)
    for (direction <- Directions()) {
      computeSurface(v, direction)
      computeSurface(v + direction, direction.neg)
    }
  }

  private def computeModel(): ModelInstance = {
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
        (v.toFloats + V3F(-0.5f, 0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        new Vector3(0, 1, 0)
      )
    for (v <- exposed(Down))
      makePBuilder.rect(
        (v.toFloats + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        new Vector3(0, -1, 0)
      )
    for (v <- exposed(North))
      makePBuilder.rect(
        (v.toFloats + V3F(-0.5f, -0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        new Vector3(0, 0, 1)
      )
    for (v <- exposed(South))
      makePBuilder.rect(
        (v.toFloats + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        new Vector3(0, 0, -1)
      )
    for (v <- exposed(East))
      makePBuilder.rect(
        (v.toFloats + V3F(0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, 0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(0.5f, -0.5f, 0.5f)).toGdx,
        new Vector3(1, 0, 0)
      )
    for (v <- exposed(West))
      makePBuilder.rect(
        (v.toFloats + V3F(-0.5f, -0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(-0.5f, 0.5f, -0.5f)).toGdx,
        (v.toFloats + V3F(-0.5f, 0.5f, 0.5f)).toGdx,
        (v.toFloats + V3F(-0.5f, -0.5f, 0.5f)).toGdx,
        new Vector3(-1, 0, 0)
      )
    new ModelInstance(builder.end())
  }

  override def getRenderables(renderables: com.badlogic.gdx.utils.Array[Renderable], pool: Pool[Renderable]): Unit =
    model().getRenderables(renderables, pool)

}
