package com.phoenixkahlo.hellcraft.core

import com.phoenixkahlo.hellcraft.math._

case class TerrainGrid(map: Map[V3I, Terrain]) {
  def terrainAt(p: V3I): Option[Terrain] =
    map.get(p)

  def terrainGridPoint(v: V3I): Option[TerrainUnit] =
    terrainAt(v / 16 floor).map(_.grid.atMod(v))

  def sampleDensity(v: V3F): Option[Float] = {
    if (v % 1 == Origin)
      terrainGridPoint(v toInts).map(mat => if (mat.id <= 0) 0f else 1f)
    else {
      // trilinear interpolation
      val v0 = v.floor
      val v1 = v0 + Ones
      if (v0.toAsSeq(v1).map(_ / 16 floor).forall(terrainAt(_).isDefined)) {
        // helper function (stands for density grid point)
        def dgp(x: V3I, y: V3I, z: V3I): Float =
          if (terrainGridPoint(V3I(x.xi, y.yi, z.zi)).get.id <= 0) 0 else 1

        val d = (v - v0) \\ (v1 - v0)

        val c00 = dgp(v0, v0, v0) * (1 - d.x) + dgp(v1, v0, v0) * d.x
        val c01 = dgp(v0, v0, v1) * (1 - d.x) + dgp(v1, v0, v1) * d.x
        val c10 = dgp(v0, v1, v0) * (1 - d.x) + dgp(v1, v1, v0) * d.x
        val c11 = dgp(v0, v1, v1) * (1 - d.x) + dgp(v1, v1, v1) * d.x

        val c0 = c00 * (1 - d.y) + c10 * d.y
        val c1 = c01 * (1 - d.y) + c11 * d.y

        val c = c0 * (1 - d.z) + c1 * d.z

        Some(c)
      } else None
    }
  }

  def sampleDirection(v: V3F): Option[V3F] =
    Directions().map(d => sampleDensity(d * 0.01f + v).map(d * _)).fold(Some(Origin))({
      case (Some(a), Some(b)) => Some(a + b)
      case _ => None
    }).map(v => (v / 6).normalize)
}