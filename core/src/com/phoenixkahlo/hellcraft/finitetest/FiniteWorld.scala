package com.phoenixkahlo.hellcraft.finitetest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Entity
import com.phoenixkahlo.hellcraft.math.{Origin, V3I}

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet

class FiniteWorld(
                   val size: V3I,
                   val chunkSize: Int,
                   val chunks: Vector[Chunk],
                   val time: Long
                 ) extends World {


  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = chunkPos >= Origin && chunkPos < size

  def this(size: V3I, chunkSize: Int = 16) =
    this(
      size, chunkSize,
      (0 until size.fold(_ * _)).map(n => new Chunk(size.decompress(n), chunkSize)).to[Vector],
      0
    )

  def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def decompress(i: Int): V3I = size.decompress(i)

  def chunkAt(v: V3I): Option[Chunk] =
    if (v >= Origin && v < size) Some(chunks(compress(v)))
    else None

  override def blockAt(v: V3I): Option[Block] = {
    chunkAt(v / chunkSize floor).flatMap(_ (v % chunkSize))
  }

  def transformChunk(v: V3I, f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.updated(
        compress(v),
        f(chunkAt(v).get)
      ),
      time
    )

  def putBlock(v: V3I, b: Block): FiniteWorld =
    transformChunk(v / chunkSize floor, _.putBlock(v % chunkSize, b))

  override def findEntity(id: UUID): Entity =
    chunks.map(_.entities.get(id)).find(_.isDefined).flatten.get

  def incrTime: FiniteWorld =
    new FiniteWorld(size, chunkSize, chunks, time + 1)

  /*
  def update: FiniteWorld =
    integrate(chunks.flatMap(_.update(this))) incrTime


  def integrate(events: Seq[ChunkEvent]): FiniteWorld = {
    val grouped = events.groupBy(_.target)
    mapChunks(c => {
      grouped.get(c.pos) match {
        case Some(e) => e.foldLeft(c)({ case (cc, ee) => ee(cc) })
        case None => c
      }
    })
  }
  */

  def update: FiniteWorld =
    integrate(chunks.flatMap(_.update(this))) incrTime

  def integrate(events: Seq[ChunkEvent]): FiniteWorld = {
    val set = events.foldLeft(new TreeSet[ChunkEvent])(_ + _)
    integrate(set)
  }

  def integrate(events: SortedSet[ChunkEvent]): FiniteWorld = {
    val grouped: Map[V3I, SortedSet[ChunkEvent]] = events.groupBy(_.target)
    mapChunks(c => {
      grouped.get(c.pos) match {
        case Some(e) => e.foldLeft(c)({ case (cc, ee) => ee(cc) })
        case None => c
      }
    })
  }

  def mapChunks(f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.par.map(f).seq,
      time
    )

  def mapBlocks(f: V3I => Block): FiniteWorld =
    mapChunks(chunk => chunk.mapBlocks(v => f(v + (chunk.pos * chunkSize))))

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    chunks.flatMap(_.renderables(texturePack, this))



}

/*
class FiniteWorld(
                   val size: V3I,
                   val chunkSize: Int,
                   val chunks: Vector[Chunk],
                   val time: Long
                 ) extends World {


  override def chunkIsDefinedAt(chunkPos: V3I): Boolean = chunkPos >= Origin && chunkPos < size

  def this(size: V3I, chunkSize: Int = 16) =
    this(
      size, chunkSize,
      (0 until size.fold(_ * _)).map(n => new Chunk(size.decompress(n), chunkSize)).to[Vector],
      0
    )

  def compress(v: V3I): Int =
    v.xi + v.zi * size.xi + v.yi * size.xi * size.zi

  def decompress(i: Int): V3I = size.decompress(i)

  def chunkAt(v: V3I): Option[Chunk] =
    if (v >= Origin && v < size) Some(chunks(compress(v)))
    else None

  override def blockAt(v: V3I): Option[Block] = {
    chunkAt(v / chunkSize floor).flatMap(_ (v % chunkSize))
  }

  def transformChunk(v: V3I, f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.updated(
        compress(v),
        f(chunkAt(v).get)
      ),
      time
    )

  def putBlock(v: V3I, b: Block): FiniteWorld =
    transformChunk(v / chunkSize floor, _.putBlock(v % chunkSize, b))

  override def findEntity(id: UUID): Entity =
    chunks.map(_.entities.get(id)).find(_.isDefined).get.get

  def incrTime: FiniteWorld =
    new FiniteWorld(size, chunkSize, chunks, time + 1)

  def update: FiniteWorld =
    integrate(chunks.flatMap(_.update(this))) incrTime

  def integrate(events: Seq[ChunkEvent]): FiniteWorld = {
    val grouped = events.groupBy(_.target)
    mapChunks(c => {
      grouped.get(c.pos) match {
        case Some(e) => e.foldLeft(c)({ case (cc, ee) => ee(cc) })
        case None => c
      }
    })
  }

  def mapChunks(f: Chunk => Chunk): FiniteWorld =
    new FiniteWorld(
      size, chunkSize,
      chunks.par.map(f).seq,
      time
    )

  def mapBlocks(f: V3I => Block): FiniteWorld =
    mapChunks(chunk => chunk.mapBlocks(v => f(v + (chunk.pos * chunkSize))))

  def renderables(texturePack: TexturePack): Seq[RenderableFactory] =
    chunks.flatMap(_.renderables(texturePack, this))



}
*/