package com.phoenixkahlo.hellcraft.save

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.Path

import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.math.{Ones, V3I}
import com.twitter.chill.{Input, Output}

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

case class RegionSave(path: Path, regionSize: Int) extends WorldSave {

  def fileName(region: V3I): String =
    "c" + region.xi + "c" + region.yi + "c" + region.zi + "c" + ".region"

  def regionOf(chunkPos: V3I): V3I = chunkPos / regionSize floor

  def chunksIn(region: V3I): Seq[V3I] = (region * regionSize) until ((region + Ones) * regionSize)

  override def save(chunks: Seq[Chunk], world: World): Unit = {
    for ((region, group) <- chunks.groupBy(c => regionOf(c.pos))) {
      // generate the chunk map
      val realChunksInRegion = chunksIn(region).filter(world chunkIsDefinedAt).to[HashSet]
      val map: Map[V3I, Chunk] =
        if (group.map(_.pos).forall(realChunksInRegion.contains)) new HashMap[V3I, Chunk]
        else load(chunksIn(region))
      val newMap = group.foldLeft(map)({ case (a, c) => a.updated(c.pos, c) })
      // save to file
      val file = path.resolve(fileName(region)).toFile
      if (!file.exists) file.createNewFile()
      val output = new Output(new FileOutputStream(file))
      GlobalKryo().writeObject(output, newMap)
      output.close()
    }
  }

  override def save(chunk: Chunk, world: World): Unit = save(Seq(chunk), world)

  val buffers = new ThreadLocal[Array[Byte]] {
    override def initialValue(): Array[Byte] = new Array[Byte](1000000)
  }

  //TODO: parrelilize
  override def load(chunksPoss: Seq[V3I]): Map[V3I, Chunk] = {
    val accumulator = new mutable.HashMap[V3I, Chunk]()
    for ((regionPos, pos) <- chunksPoss.groupBy(regionOf(_))) {
      val file = path.resolve(fileName(regionPos)).toFile
      if (file.exists) {
        //val input = new Input(new FileInputStream(file))
        //input.setBuffer(new Array[Byte](4096))
        val input = new Input(100000)
        input.setBuffer(buffers.get())
        input.setInputStream(new FileInputStream(file))

        val region = GlobalKryo().readObject(input, classOf[Map[V3I, Chunk]])
        input.close()
        for (chunk <- region.values)
          accumulator.put(chunk.pos, chunk)
      }
    }
    accumulator.toMap
  }

  override def load(chunkPos: V3I): Option[Chunk] = load(Seq(chunkPos)).get(chunkPos)

}
