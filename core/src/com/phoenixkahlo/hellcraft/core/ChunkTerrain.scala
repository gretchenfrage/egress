package com.phoenixkahlo.hellcraft.core

import java.io._
import java.lang.reflect.{Field, Modifier}
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.util.{ByteField, VectorField}
import com.phoenixkahlo.hellcraft.math.{Origin, Quad, V3I}
import com.phoenixkahlo.hellcraft.util.ParamCache

/*
@CarboniteWith(classOf[FieldNode])
case class TerrainField(materials: ByteField, densities: ByteField)
*/

class ChunkTerrain(chunk: Chunk) {

  val verts = VectorField(v => {
    ???
  })

  val surface = new ParamCache[World, Seq[Quad]](world => {
    if (!chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      throw new IllegalArgumentException("cannot compute terrain without neighbors")

    ???
  })

  /*
  val surface = new ParamCache[World, Seq[Quad]](world => {
    if (!chunk.pos.neighbors.forall(world.chunkAt(_).isDefined))
      throw new IllegalArgumentException("cannot compute terrain without neighbors")


  })
  */

}