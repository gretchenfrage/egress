package com.phoenixkahlo.hellcraft.serial

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import java.util.zip.Deflater

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, KryoException, Serializer, io}
import com.phoenixkahlo.hellcraft.core.{Air, Brick, Chunk}
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.collection.SortedMap
import scala.collection.mutable.ArrayBuffer

case class InterframeCompressedChunks(frames: SortedMap[Long, Either[Chunk, Map[UUID, Entity]]])

object InterframeCompressedChunks {

  def compress(frames: SortedMap[Long, Chunk]): InterframeCompressedChunks = {
    if (frames isEmpty) InterframeCompressedChunks(SortedMap.empty)

    var compressed: SortedMap[Long, Either[Chunk, Map[UUID, Entity]]] = SortedMap(frames.firstKey -> Left(frames.head._2))
    var lastGrid = frames.head._2.blocks

    for ((t, chunk) <- frames) {
      if (chunk.blocks == lastGrid) {
        compressed = compressed.updated(t, Right(chunk.entities))
      } else {
        compressed = compressed.updated(t, Left(chunk))
        lastGrid = chunk.blocks
      }
    }

    InterframeCompressedChunks(compressed)
  }

    def decompress(compressed: InterframeCompressedChunks): SortedMap[Long, Chunk] = {
      if (compressed.frames isEmpty) SortedMap.empty

      var frames: SortedMap[Long, Chunk] =
        SortedMap(compressed.frames.firstKey -> compressed.frames.head._2.asInstanceOf[Left[Chunk, _]].value)

      for ((t, either) <- compressed.frames) {
        either match {
          case Left(chunk) => frames = frames.updated(t, chunk)
          case Right(entities) => frames = frames.updated(t, frames.last._2.copy(entities = entities))
        }
      }

      frames
    }

}

object InterframeChunkCompressionTest {
  def test(): Unit = {
    val chunk = new Chunk(V3I(0, 0, 0)).mapBlocks(v => if (v.hashCode % 2 == 0) Brick else Air)
    val chunks = SortedMap.empty ++ Stream.iterate((0: Long, chunk))({ case (t, c) => (t + 1, c) }).take(40).toMap

    {
      println("testing no-compression")
      val kryo = GlobalKryo.create()
      val baos = new ByteArrayOutputStream
      val output = new io.Output(baos)
      kryo.writeClassAndObject(output, chunks)
      val arr = baos.toByteArray
      println("no-compression serialized with size " + arr.length + " bytes")
      val in = new ByteArrayInputStream(arr)
      val input = new io.Input(in)
      val deserialized = kryo.readClassAndObject(input)
      if (deserialized == chunks)
        println("serialized correctly")
      else
        println("equality check failed")
    }

    {
      println("compressing " + chunks.size + " chunks")
      val compressed = InterframeCompressedChunks.compress(chunks)
      val kryo = GlobalKryo.create()
      val baos = new ByteArrayOutputStream
      val output = new io.Output(baos)
      kryo.writeClassAndObject(output, compressed)
      val arr = baos.toByteArray
      println("compressed into " + arr.size + " bytes")
      val in = new ByteArrayInputStream(arr)
      val input = new io.Input(in)
      val deserialized = kryo.readClassAndObject(input)
      if (deserialized == compressed)
        println("serialized correctly")
      else
        println("equality check failed")
    }


  }
}