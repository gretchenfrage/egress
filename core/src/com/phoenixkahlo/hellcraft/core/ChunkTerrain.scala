package com.phoenixkahlo.hellcraft.core

import java.io._
import java.lang.reflect.{Field, Modifier}
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode
import com.phoenixkahlo.hellcraft.core.util.{ByteField, VectorField}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.ParamCache

import scala.collection.mutable.ArrayBuffer

/*
@CarboniteWith(classOf[FieldNode])
case class TerrainField(materials: ByteField, densities: ByteField)
*/

object ChunkTerrain {

  val size = V3I(32, 32, 32)

  val edges: Seq[(V3I, V3I)] = Seq(
    (Origin, Up),
    (Origin, North),
    (Origin, East),
    (Ones, Ones + Down),
    (Ones, Ones + South),
    (Ones, Ones + West),
    (Up, Up + North),
    (Up, Up + East),
    (North, North + Up),
    (North, North + East),
    (East, East + Up),
    (East, East + North)
  )

  val threshhold = 128

}

class ChunkTerrain(chunk: Chunk) {

  /*
  val verts = VectorField(ChunkTerrain.size, v => {
    // interpolated surface nets algorithm
    val points = new ArrayBuffer[V3F]

    // for each pair of vertices forming
    for ((v1, v2) <- ChunkTerrain.edges.map({ case (v1, v2) => (v1 + v, v2 + v) })) {
      // if the edge
      if ((chunk.densities(v1).get >= ChunkTerrain.threshhold) ^ (chunk.densities(v2).get >= ChunkTerrain.threshhold)) {

      }
    }
    */


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