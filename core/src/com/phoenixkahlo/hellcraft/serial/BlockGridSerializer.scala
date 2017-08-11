package com.phoenixkahlo.hellcraft.serial

import java.util.zip.{Deflater, Inflater}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.phoenixkahlo.hellcraft.core.{Air, Block, BlockGrid, Stone}

class BlockGridSerializer extends Serializer[BlockGrid] {

  override def write(kryo: Kryo, output: Output, obj: BlockGrid): Unit = {
    // write heuristic
    if (obj eq BlockGrid.AirGrid) {
      output.writeBoolean(true)
      kryo.writeObject(output, Air)
      return
    } else if (obj eq BlockGrid.StoneGrid) {
      output.writeBoolean(true)
      kryo.writeObject(output, Stone)
      return
    } else {
      output.writeBoolean(false)
    }
    // compress
    val blocks = obj.blocks.toArray
    val buffer = GridCompressBuffers.get
    val compressor = new Deflater
    compressor.setInput(blocks)
    compressor.finish()
    val length = compressor.deflate(buffer)
    // write
    output.writeShort(length)
    output.writeBytes(buffer, 0, length)
  }

  override def read(kryo: Kryo, input: Input, t: Class[BlockGrid]): BlockGrid = {
    // read heuristic
    if (input.readBoolean()) {
      kryo.readObject(input, classOf[Block]) match {
        case Air => BlockGrid.AirGrid
        case Stone => BlockGrid.StoneGrid
      }
    }
    // read
    val length = input.readShort()
    val readBuffer = GridReadBuffers.get
    input.readBytes(readBuffer, 0, length)
    // decompress
    val decompressor = new Inflater
    val decompressBuffer = GridCompressBuffers.get
    decompressor.setInput(readBuffer, 0, length)
    decompressor.inflate(decompressBuffer)
    // create
    BlockGrid((0 until 4096).foldLeft(Vector[Byte]())((v, i) => v :+ decompressBuffer(i)))
  }

}

private object GridReadBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](8192)
}

private object GridCompressBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](8192)
}

/*
class ChunkSerializer extends Serializer[Chunk] {

  override def write(kryo: Kryo, output: Output, chunk: Chunk): Unit = {
    // compress the block grid
    val blocks = chunk.blocks.toArray
    val buffer = ChunkBuffers.get()
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
    val buffer = ChunkBuffers.get()
    val length = decompressor.inflate(buffer)
    decompressor.end()
    val decompressed = buffer.slice(0, length)
    val blocks = buffer.toVector
    // create
    new Chunk(pos, size, blocks, entities, null, true)
  }

}

private object ChunkBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](4096)
}
*/