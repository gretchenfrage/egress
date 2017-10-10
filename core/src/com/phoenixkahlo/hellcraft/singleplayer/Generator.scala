package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Cube
import com.phoenixkahlo.hellcraft.graphics.StoneTID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.fields.{ByteField, ByteFractionField, FloatField}
import com.phoenixkahlo.hellcraft.util.threading.{Fut, UniExecutor}

import scala.collection.mutable

class Generator(res: Int) {

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

  def genChunk(p: V3I): Fut[Chunk] = {
    heightsAt(p.flatten).map(heights => {
      new Chunk(p, Densities(p,
                    ByteField(rv3d, i => {
                      val v = p * res + i
                      val depth = (p.yi * res + i.yi) - heights(i.flatten)
                      if (depth >= 0) Air.id
                      else if (p.flatten % 2 == Origin2D) Stone.id
                      else Dirt.id
                    }),
                    FloatField(rv3d, i => {
                      val v = p * res + i
                      val depth = (p.yi * res + i.yi) - heights(i.flatten)
                      if (depth >= 0) 0
                      else 1
                    })))
    })
  }

}
