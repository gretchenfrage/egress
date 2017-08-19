package com.phoenixkahlo.hellcraft.singleplayer

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.{Executors, ThreadPoolExecutor, TimeUnit}

import com.phoenixkahlo.hellcraft.oldcore.{Air, BlockGrid, Chunk, Stone}
import com.phoenixkahlo.hellcraft.math.{V2F, V3I}
import com.phoenixkahlo.hellcraft.threading.{Fut, UniExecutor}
import other.OpenSimplexNoise

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class Generator {

  val noise = new OpenSimplexNoise

  val amp = 10f
  val scale = 8f

  class HeightPatch {
    val heights = new Array[Double](256)

    def apply(x: Int, z: Int) = heights(z * 16 + x)

    def update(x: Int, z: Int, d: Double) = heights(z * 16 + x) = d

    var min: Double = Double.NaN
    var max: Double = Double.NaN
  }

  val heightsMap = new mutable.HashMap[(Int, Int), Fut[HeightPatch]]
  val heightsLock = new ReentrantReadWriteLock

  def heightsAt(x: Int, z: Int): Fut[HeightPatch] = {
    heightsLock.readLock().lock()
    if (heightsMap.contains((x, z))) {
      heightsLock.readLock().unlock()
      return heightsMap((x, z))
    }
    heightsLock.readLock().unlock()
    heightsLock.writeLock().lock()
    val future = heightsMap.get((x, z)) match {
      case Some(future) => future
      case None =>
        val future = Fut[HeightPatch]({
          val patch = new HeightPatch
          for {
            xx <- 0 until 16
            zz <- 0 until 16
          } yield patch(xx, zz) = noise.eval((x * 16 + xx) / scale, (z * 16 + zz) / scale) * amp
          patch.min = patch.heights.min
          patch.max = patch.heights.max
          patch
        }, UniExecutor.exec(V2F(x, z) * 16 + V2F(8, 8)))
        heightsMap.put((x, z), future)
        future
    }
    heightsLock.writeLock().unlock()
    future
  }

  def genChunk(p: V3I): Fut[Chunk] = {
    heightsAt(p.xi, p.zi).map(heights => {
      if (heights.min > p.yi * 16 + 16) BlockGrid.StoneGrid
      if (heights.max < p.yi * 16) BlockGrid.AirGrid
      new Chunk(p, BlockGrid(v => {
        val depth = (p.yi * 16 + v.yi) - heights(v.xi, v.zi)
        if (depth >= 0) Air
        else Stone
      }))
    }, UniExecutor.exec(p * 16 + V3I(8, 8, 8)))
  }

}
