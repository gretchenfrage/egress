package com.phoenixkahlo.hellcraft.singleplayer

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.{Executors, ThreadPoolExecutor, TimeUnit}

import com.phoenixkahlo.hellcraft.core.{Air, BlockGrid, Chunk, Stone}
import com.phoenixkahlo.hellcraft.math.V3I
import other.OpenSimplexNoise

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class Generator {

  implicit val workContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors,
    runnable => {
      val thread = new Thread(runnable, "generator thread")
      thread.setPriority(3)
      thread
    }))
  val noise = new OpenSimplexNoise

  val amp = 1f
  val scale = 1f

  class HeightPatch {
    val heights = new Array[Double](256)

    def apply(x: Int, z: Int) = heights(z * 16 + x)

    def update(x: Int, z: Int, d: Double) = heights(z * 16 + x) = d

    var min: Double = Double.NaN
    var max: Double = Double.NaN
  }

  val heightsMap = new mutable.HashMap[(Int, Int), Future[HeightPatch]]
  val heightsLock = new ReentrantReadWriteLock

  def heightsAt(x: Int, z: Int): Future[HeightPatch] = {
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
        val future = Future[HeightPatch] {
          val patch = new HeightPatch
          for {
            xx <- 0 until 16
            zz <- 0 until 16
          } yield patch(xx, zz) = noise.eval((x * 16 + xx) / scale, (z * 16 + zz) / scale) * amp
          patch.min = patch.heights.min
          patch.max = patch.heights.max
          patch
        }(workContext)
        heightsMap.put((x, z), future)
        future
    }
    heightsLock.writeLock().unlock()
    future
  }

  def genChunk(p: V3I): Future[Chunk] = {
    heightsAt(p.xi, p.zi).map(heights => {
      if (heights.min > p.yi * 16 + 16) BlockGrid.StoneGrid
      if (heights.max < p.yi * 16) BlockGrid.AirGrid
      new Chunk(p, BlockGrid(v => {
        val depth = (p.yi * 16 + v.yi) - heights(v.xi, v.zi)
        if (depth >= 0) Air
        else Stone
      }))
    })
  }
  /*

  def generate(p: V3I): BlockGrid = {
    val heights = heightsAt(p.xi, p.zi)
    if (heights.min > p.yi * 16 + 16) BlockGrid.StoneGrid
    if (heights.max < p.yi * 16) BlockGrid.AirGrid
    BlockGrid(v => {
      val depth = (p.yi * 16 + v.yi) - heights(v.xi, v.zi)
      if (depth >= 0) Air
      else Stone
    })
  }

  def genChunk(p: V3I): Future[Chunk] =
    Future { new Chunk(p, generate(p)) }

  val heightsMap = new mutable.ParHashMap[(Int, Int), Either[HeightPatch, Future[HeightPatch]]]
  val heightsLock = new ReentrantReadWriteLock

  def heightsAt(x: Int, z: Int): HeightPatch = {
    heightsLock.readLock().lock()
    heightsMap.get((x, z)) match {
      case Some(Left(patch)) => return patch
      case Some(Right(future)) => return Await.result(future, Duration.Inf)
      case None =>
    }
    heightsLock.readLock().unlock()
    heightsLock.writeLock().lock()
    heightsMap.get((x, z)) match {
      case Some(Left(patch)) => return patch
      case Some(Right(future)) => return Await.result(future, Duration.Inf)
      case None =>
    }
    val future = Future[HeightPatch] {
      val patch = new HeightPatch
      for {
        xx <- 0 until 16
        zz <- 0 until 16
      } yield patch(xx, zz) = noise.eval((x * 16 + xx) / scale, (z * 16 + zz) / scale) * amp
      patch.min = patch.heights.min
      patch.max = patch.heights.max
      patch
    } (workContext)
    heightsMap.put((x, z), Right(future))
    heightsLock.writeLock().unlock()
    val patch = Await.result(future, Duration.Inf)
    heightsLock.writeLock().lock()
    heightsMap.put((x, z), Left(patch))
    heightsLock.writeLock().unlock()
    patch
  }



    */

}
