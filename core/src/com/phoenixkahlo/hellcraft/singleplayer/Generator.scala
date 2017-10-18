package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Cube
import com.phoenixkahlo.hellcraft.graphics.StoneTID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields.{ByteField, ByteFractionField, FloatField, IDField}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.mutable

class Generator(res: Int) {

  implicit val mapping = TerrainUnits

  val rv2d = V2I(res, res)
  val rv3d = V3I(res, res, res)

  val heightNoise = Simplex(1f / 8f, 15f)

  class HeightPatch {
    val heights = new Array[Double](res * res)

    def apply(v: V2I) = heights(v.yi * res + v.xi)

    def update(v: V2I, d: Double) = heights(v.yi * res + v.xi) = d

    var min: Double = Double.NaN
    var max: Double = Double.NaN
  }

  val heightsMap = new mutable.HashMap[V2I, Fut[HeightPatch]]
  val heightsLock = new ReentrantReadWriteLock

  def heightsAt(v: V2I): Fut[HeightPatch] = {
    heightsLock.readLock().lock()
    if (heightsMap.contains(v)) {
      try return heightsMap(v)
      finally heightsLock.readLock().unlock()
    }
    heightsLock.readLock().unlock()
    heightsLock.writeLock().lock()
    val fut = heightsMap.get(v) match {
      case Some(fut) => fut
      case None =>
        val fut = Fut[HeightPatch]({
          val patch = new HeightPatch
          for (vv <- Origin2D until rv2d) {
            patch(vv) = heightNoise(v * res + vv)
          }
          patch.min = patch.heights.min
          patch.max = patch.heights.max
          patch
        }, UniExecutor.exec(v * 16 + V2I(8, 8)))
        heightsMap.put(v, fut)
        fut
    }
    heightsLock.writeLock().unlock()
    fut
  }

  val range = V3I(70, 40, 80)
  val seed = 98746513214983L//987465216454653L
  val poss = RNG.v3fs(RNG(seed)).map(_ ** range)//.take(15).to[Vector]
  val rads = RNG.floats(RNG(seed ^ 2789354325L)).map(_ * 7 + 5)
  val balls = poss.zip(rads).take(15)
  //println(balls)
  //val rad = 7

  def meta(v: V3F): Float =
    balls.map({ case (b, rad) => (rad * rad) / (v - b).magnitudeSqrd }).sum

  val noise = Simplex(0.1f, 1)

  val simpFrac = 1.5f
  def simp(v: V3F): Float =
    noise(v) / simpFrac + (1f - 1f / simpFrac)

  /*
  def genChunk(p: V3I): Fut[Chunk] =
    Fut[Chunk](new Chunk(p, Terrain(p, IDField[TerrainUnit](rv3d, (i: V3I) => {
      val v = p * WorldRes + i
      val factor = V3F(1, 1.5f, 1)
      if (simp(v ** factor / 1f) * meta(v ** factor / 1f) > 0.5f) {
        Materials.Stone
      } else Air
    }))), UniExecutor.exec(p * 16 + Repeated(8)))
*/


  def genChunk(p: V3I): Fut[Chunk] = {
    heightsAt(p.flatten).map(heights => {
      new Chunk(p, Terrain(p, IDField[TerrainUnit](rv3d, (i: V3I) => {
              val depth = (p.yi * res + i.yi) - heights(i.flatten)
              if (depth >= 0) Air
              else if (p.flatten % 2 == Origin2D) Materials.Stone
              else Materials.Grass
            })))
    }, UniExecutor.exec(p * 16 + Repeated(8)))
  }

  /*
  val noise = Simplex(0.1f, 1)
  def genChunk(p: V3I): Fut[Chunk] =
    Fut[Chunk](new Chunk(p, Terrain(p, IDField[TerrainUnit](rv3d, (i: V3I) => {
      if (noise(p * 16 + i) > 0.5f) Materials.Stone else Air
    }))), UniExecutor.exec(p * 16 + Repeated(8)))
    */

}
