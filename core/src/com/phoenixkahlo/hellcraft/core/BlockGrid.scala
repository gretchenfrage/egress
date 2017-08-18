package com.phoenixkahlo.hellcraft.core

import java.io._
import java.lang.reflect.{Field, Modifier}
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

case class BlockGrid private(private var blocks: Either[Array[Byte], Vector[Byte]]) extends Externalizable {

  private def this(blocks: Array[Byte]) = this(Left(blocks))
  private def this(blocks: Vector[Byte]) = this(Right(blocks))

  // TODO: don't reevaluate this with every modification
  //lazy val isAllOpaque: Boolean = blocks.map(BlockDirectory.lookup).forall(_ isOpaque)
  //lazy val isAllTranslucent: Boolean = !isAllOpaque && blocks.map(BlockDirectory.lookup).forall(_ isTranslucent)
  lazy val isAllOpaque: Boolean = blocks match {
    case Left(arr) => arr.forall(BlockDirectory.lookup(_).isOpaque)
    case Right(vec) => vec.forall(BlockDirectory.lookup(_).isOpaque)
  }
  lazy val isAllTranslucent: Boolean = !isAllOpaque && (blocks match {
    case Left(arr) => arr.forall(BlockDirectory.lookup(_).isTranslucent)
    case Right(vec) => vec.forall(BlockDirectory.lookup(_).isTranslucent)
  })

  private def asVector: Vector[Byte] = blocks match {
    case Left(arr) => (0 until 4096).foldLeft(Vector[Byte]())((v, i) => v :+ arr(i))
    case Right(vec) => vec
  }

  private def asArray: Array[Byte] = blocks match {
    case Left(arr) => arr
    case Right(vec) => vec.toArray
  }


  def updated(v: V3I, b: Block): BlockGrid = {
    new BlockGrid(asVector.updated(BlockGrid.compress(v), b.id))
  }

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < V3I(16, 16, 16)) blocks match {
      case Left(arr) => Some(BlockDirectory(arr(BlockGrid.compress(v))))
      case Right(vec) => Some(BlockDirectory(vec(BlockGrid.compress(v))))
    }
    else None

  override def writeExternal(out: ObjectOutput): Unit = {
    val compressor = new Deflater
    compressor.setInput(asArray)
    compressor.finish()
    val buffer = GridCompressBuffers.get
    val length = compressor.deflate(buffer)
    out.writeShort(length)
    out.write(buffer, 0, length)
  }

  override def readExternal(in: ObjectInput): Unit = {
    val length = in.readShort()
    val readBuffer = GridReadBuffers.get
    in.read(readBuffer, 0, length)
    val decompressor = new Inflater
    val decompressBuffer = GridCompressBuffers.get
    decompressor.setInput(readBuffer, 0, length)
    decompressor.inflate(decompressBuffer)
    blocks = Left(decompressBuffer.slice(0, 4096))
  }

  override def toString: String = {
    trait PrintMode
    object PrintAllBlocks extends PrintMode
    object PrintHashCode extends PrintMode
    object PrintIdentity extends PrintMode
    val mode: PrintMode = PrintHashCode
    mode match {
      case PrintAllBlocks => super.toString
      case PrintIdentity => System.identityHashCode(this).toString
      case PrintHashCode => hashCode().toString
    }
  }

}

private object GridReadBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](8192)
}

private object GridCompressBuffers extends ThreadLocal[Array[Byte]] {
  override def initialValue(): Array[Byte] = new Array[Byte](8192)
}

object BlockGrid {

  def apply(block: Block): BlockGrid = {
    val arr = new Array[Byte](4096)
    for (i <- arr.indices)
      arr(i) = block.id
    new BlockGrid(arr)
    //new BlockGrid(Stream.iterate(block.id, 4096)(identity).toArray)
    //new BlockGrid((1 to 4096).foldLeft(Vector[Byte]())((v, _) => v :+ block.id))
  }

  def apply(generator: V3I => Block): BlockGrid = {
    val arr = new Array[Byte](4096)
    for (i <- arr.indices)
      arr(i) = generator(decompress(i)).id
    new BlockGrid(arr)
    //new BlockGrid((0 until 4096).map(i => generator(decompress(i)).id).to[Vector])
  }

  def compress(v: V3I): Int =
    v.xi + v.zi * 16 + v.yi * 256

  def decompress(i: Int): V3I =
    V3I(
      x = i % 16,
      y = i / 256,
      (i % 256) / 16
    )

  val AirGrid = BlockGrid(Air)
  val StoneGrid = BlockGrid(Stone)

}