package com.phoenixkahlo.hellcraft.save

import java.io.ByteArrayOutputStream
import java.util.UUID
import java.util.zip.{Deflater, Inflater}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.phoenixkahlo.hellcraft.core.Chunk
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.V3I

class ChunkSerializer extends Serializer[Chunk] {

  override def write(kryo: Kryo, output: Output, chunk: Chunk): Unit = {
    // compress the block grid
    val blocks = chunk.blocks.toArray
    val buffer = SerialBuffers.get()
    val compressor = new Deflater
    compressor.setInput(blocks)
    compressor.finish()
    val length = compressor.deflate(buffer)
    val compressed = buffer.slice(0, length)
    // serialize
    kryo.writeObject(output, chunk.pos)
    kryo.writeObject(output, chunk.size)
    kryo.writeObject(output, compressed)
    kryo.writeObject(output, chunk.entities)
  }

  override def read(kryo: Kryo, input: Input, t: Class[Chunk]): Chunk = {
    // deserialize
    val pos = kryo.readObject(input, classOf[V3I])
    val size = kryo.readObject(input, classOf[Int])
    val compressed = kryo.readObject(input, classOf[Array[Byte]])
    val entities = kryo.readObject(input, classOf[Map[UUID,Entity]])
    // decompress
    val decompressor = new Inflater
    decompressor.setInput(compressed, 0, compressed.length)
    val buffer = SerialBuffers.get()
    val length = decompressor.inflate(buffer)
    decompressor.end()
    val decompressed = buffer.slice(0, length)
    val blocks = buffer.toVector
    // create
    new Chunk(pos, size, blocks, entities, null, true)
  }

}

object SerialBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](4096)
}