package com.phoenixkahlo.hellcraft.save

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Path

import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.core.{Chunk, World}
import com.phoenixkahlo.hellcraft.math.V3I

/**
  * A world save implementation that keeps each chunk in its own file, serializing them with the Kryo.
  */
case class NaiveWorldSave(path: Path) extends WorldSave {

  def fileName(chunkPos: V3I): String =
    "c" + chunkPos.x + "c" + chunkPos.y + "c" + chunkPos.z + "c" + ".dat"


  override def save(chunks: Seq[Chunk], world: World): Unit =
    for (chunk <- chunks) save(chunk, world)

  override def save(chunk: Chunk, world: World): Unit = {
    val file = path.resolve(fileName(chunk.pos)).toFile
    if (!file.exists) file.createNewFile()
    val output = new Output(new FileOutputStream(file))
    GlobalKryo().writeObject(output, chunk)
    output.close()
  }

  override def load(chunksPoss: Seq[V3I]): Map[V3I, Chunk] =
    chunksPoss.map(p => (p, load(p))).toMap.filter({ case (_, o) => o isDefined }).mapValues(_.get)

  override def load(chunkPos: V3I): Option[Chunk] = {
    val file = path.resolve(fileName(chunkPos)).toFile
    if (file.exists) {
      val input = new Input(new FileInputStream(file))
      val chunk = Some(GlobalKryo().readObject(input, classOf[Chunk]))
      input.close()
      chunk
    } else None
  }

}
