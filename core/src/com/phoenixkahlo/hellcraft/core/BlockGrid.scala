package com.phoenixkahlo.hellcraft.core

import java.io.{Externalizable, ObjectInput, ObjectOutput}
import java.lang.reflect.{Field, Modifier}
import java.util.zip.{Deflater, Inflater}

import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

@SerialVersionUID(2384654234L)
case class BlockGrid(blocks: Vector[Byte]) extends Externalizable {

  def this() = this(null)

  // TODO: don't reevaluate this with every modification
  lazy val isAllOpaque: Boolean = blocks.map(BlockDirectory.lookup).forall(_ isOpaque)
  lazy val isAllTranslucent: Boolean = !isAllOpaque && blocks.map(BlockDirectory.lookup).forall(_ isTranslucent)

  def updated(v: V3I, b: Block): BlockGrid = {
    new BlockGrid(blocks.updated(BlockGrid.compress(v), b.id))
  }

  def apply(v: V3I): Option[Block] =
    if (v >= Origin && v < V3I(16, 16, 16)) Some(BlockDirectory(blocks(BlockGrid.compress(v))))
    else None

  override def writeExternal(out: ObjectOutput): Unit = {
    val compressor = new Deflater
    compressor.setInput(blocks.toArray)
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
    val blocks = (0 until 4096).foldLeft(Vector[Byte]())((v, i) => v :+ decompressBuffer(i))
    BlocksFieldSetter.set(this, blocks)
  }

}

private object BlocksFieldSetter {

  private val field = {
    val blocksField = classOf[BlockGrid].getDeclaredField("blocks")
    val modsFields = classOf[Field].getDeclaredField("modifiers")
    modsFields.setAccessible(true)
    modsFields.setInt(blocksField, blocksField.getModifiers & ~Modifier.FINAL)
    blocksField.setAccessible(true)
    blocksField
  }

  def set(grid: BlockGrid, blocks: Vector[Byte]): Unit = {
    field.set(grid, blocks)
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
    this((1 to 4096).foldLeft(Vector[Byte]())((v, _) => v :+ block.id))
  }

  def apply(generator: V3I => Block): BlockGrid = {
    this((0 until 4096).map(i => generator(decompress(i)).id).to[Vector])
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