package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Cube
import com.phoenixkahlo.hellcraft.graphics.StoneTID
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.collections.MemoFunc
import com.phoenixkahlo.hellcraft.util.fields._
import com.phoenixkahlo.hellcraft.util.threading.{Fut, MergeFut, UniExecutor}

import scala.collection.mutable

trait Generator {
  val chunkAt: V3I => Fut[Chunk]

  def cancel(): Unit
}

class DefaultGenerator(res: Int) extends Generator {

  implicit val mapping: IDMapping[TerrainUnit] = TerrainUnits
  private val noise = Simplex(1f / 8f, 15f)

  @volatile private var cancelled = false

  val heightAt = new MemoFunc[V2I, Fut[FloatField]](v =>
    if (!cancelled) {
      Fut(FloatField(V3I(res, 1, res),
        i => noise(v * res + i.flatten)
      ), UniExecutor.exec(v * 16))
    } else Fut(null: FloatField, _.run())
  )

  val terrainAt = new MemoFunc[V3I, Fut[Terrain]](p =>
    if (!cancelled) {
      heightAt(p.flatten).map(
        height => Terrain(p, IDField[TerrainUnit](V3I(res, res, res), (i: V3I) => {
          val depth = (p.yi * res + i.yi) - height(V3I(i.xi, 0, i.zi))
          if (depth >= 0) Air
          else if (p.flatten % 2 == Origin2D) Blocks.Stone
          else Materials.Grass
        }))
      , UniExecutor.exec(p * 16))
    } else Fut(null: Terrain, _.run())
  )

  val soupAt = new MemoFunc[V3I, Fut[(Terrain, BlockSoup, TerrainSoup)]](p =>
    if (!cancelled) {
      p.neighbors
        .map(terrainAt)
        .foldLeft(Fut(Map.empty[V3I, Terrain], _.run()))(
          (accumFut: Fut[Map[V3I, Terrain]], terrFut: Fut[Terrain]) => MergeFut(accumFut, terrFut,
            (accum: Map[V3I, Terrain], terr: Terrain) => accum + (terr.pos -> terr)
          )(_.run())
        )
        .map(terrains => {
          val grid = new TerrainGrid {
            override def terrainAt(p: V3I): Option[Terrain] = terrains.get(p)
          }
          val ter = terrains(p)
          (ter, BlockSoup(ter, grid).get, TerrainSoup(ter, grid).get)
        }, UniExecutor.exec(p * 16))
    } else Fut(null: (Terrain, BlockSoup, TerrainSoup), _.run())
  )



  /*
  override val chunkAt = new MemoFunc[V3I, Fut[Chunk]](p =>
    if (!cancelled) {
      heightAt(p.flatten).map(
        height => new Chunk(p,
          Terrain(p, IDField[TerrainUnit](V3I(res, res, res), (i: V3I) => {
            val depth = (p.yi * res + i.yi) - height(V3I(i.xi, 0, i.zi))
            if (depth >= 0) Air
            else if (p.flatten % 2 == Origin2D) Blocks.Stone
            else Materials.Grass
          }))
        )
        , UniExecutor.exec(p * 16))
    } else Fut(null: Chunk, _.run())
  )
  */

  override val chunkAt = new MemoFunc[V3I, Fut[Chunk]](p =>
    if (!cancelled) {
      soupAt(p).map({ case (ter, bs, ts) => new Chunk(p, ter, Map.empty, ts, bs, None, None) }, UniExecutor.exec(p * 16))
    } else Fut(null: Chunk, _.run())
  )

  override def cancel(): Unit = {
    cancelled = true
  }
}
