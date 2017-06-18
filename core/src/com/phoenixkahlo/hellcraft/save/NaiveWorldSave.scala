package com.phoenixkahlo.hellcraft.save

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Path

import com.esotericsoftware.kryo.io.{Input, Output}
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.math.V3I
import com.phoenixkahlo.hellcraft.util.GlobalKryo

/**
  * A world save implementation that keeps each chunk in its own file, serializing them with the Kryo.
  */
case class NaiveWorldSave(path: Path) extends WorldSave {

  def fileName(chunkPos: V3I): String =
    "c" + chunkPos.x + "c" + chunkPos.y + "c" + chunkPos.z + "c" + ".dat"

  override def save(chunk: Chunk): Unit = {
    GlobalKryo.kryo.synchronized {
      val file = path.resolve(fileName(chunk.pos)).toFile
      if (!file.exists) file.createNewFile()
      val output = new Output(new FileOutputStream(file))
      GlobalKryo.kryo.writeObject(output, chunk)
      output.close()
    }
  }

  override def load(chunkPos: V3I): Option[Chunk] = {
    GlobalKryo.kryo.synchronized {
      val file = path.resolve(fileName(chunkPos)).toFile
      if (file.exists) {
        val input = new Input(new FileInputStream(file))
        val chunk = Some(GlobalKryo.kryo.readObject(input, classOf[Chunk]))
        input.close()
        chunk
      } else None
    }
  }

}
