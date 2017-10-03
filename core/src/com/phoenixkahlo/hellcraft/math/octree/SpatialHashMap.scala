package com.phoenixkahlo.hellcraft.math.octree

import com.phoenixkahlo.hellcraft.math.{Origin, Trig, V3F, V3I}

import scala.collection.mutable.ArrayBuffer

case class SpatialHashMap[+E] private(binSize: Float, searchRange: Float, bins: Map[V3I, List[(V3F, E)]]) extends Map[V3F, E] {
  def hash(key: V3F): V3I = key / binSize floor

  override def +[V1 >: E](kv: (V3F, V1)): SpatialHashMap[V1] = {
    val (key, value) = kv
    val hashed = hash(key)
    copy(bins = bins + (hashed -> (bins.getOrElse(hashed, List.empty).filterNot(_._1 == key) :+ kv)))
  }

  override def -(key: V3F): SpatialHashMap[E] = {
    val hashed = hash(key)
    val newList = bins.getOrElse(hashed, List.empty).filterNot(_._1 == key)
    if (newList isEmpty)
      copy(bins = bins - hashed)
    else
      copy(bins = bins.updated(hashed, newList))
  }

  override def get(key: V3F): Option[E] = {
    bins.getOrElse(hash(key), List.empty).find({ case (k, v) => k == key }).map(_._2)
  }

  override def iterator: Iterator[(V3F, E)] = {
    bins.values.iterator.flatten
  }

  def closest(point: V3F): Option[(V3F, E)] = {
    if (isEmpty) None
    else {
      val sortedBins: Seq[(Float, (V3I, List[(V3F, E)]))] =
        bins.toSeq.map(kv => ((kv._1 * binSize) dist point) -> kv).sortBy(_._1)
      val closestBinDist: Float =
        sortedBins.head._1
      val possibleBins: Seq[List[(V3F, E)]] =
        sortedBins.takeWhile(_._1 <= (closestBinDist + searchRange)).map(_._2._2)
      Some(possibleBins.flatten.minBy(_._1 dist point))
    }
  }

  override def size: Int = {
    bins.values.map(_.size).sum
  }
}

object SpatialHashMap {
  def apply[E](binSize: Float): SpatialHashMap[E] = {
    val searchRange = Math.sqrt(3 * binSize * binSize).toFloat * 2.001f
    SpatialHashMap(binSize, searchRange, Map.empty)
  }
}

object SpatialHashMapTest extends App {
  for (pow <- -2 to 5) {
    val binSize = Math.pow(2, pow).toFloat
    println("binSize = " + binSize)
    val t1 = System.nanoTime()

    var map: SpatialHashMap[Unit] = SpatialHashMap(binSize)
    for (_ <- 1 to 1000) {
      map += ((V3F(
        Math.random().toFloat * 12 - 6,
        Math.random().toFloat * 12 - 6,
        Math.random().toFloat * 12 - 6
      ), ()))
    }
    //println(map)
    val original = map
    println("size = " + map.size)
    val pos = Origin
    val buffer = new ArrayBuffer[V3F]
    while (map nonEmpty) {
      val item = map.closest(pos).get._1
      buffer += item
      map -= item
    }
    //println(buffer)

    val t2 = System.nanoTime()
    println("construction and deconstruction took " + (t2 - t1) / 1000000 + " ms")

    val vector = buffer.to[Vector]
    val sorted = vector.sortBy(_ dist pos)
    println("is sorted = " + (vector == sorted))
    if (vector != sorted) {
      println("unsorted!")
      for {
        (bin, list) <- original.bins
        (key, value) <- list
      } yield {
        if (original.hash(key) != bin) {
          println("(" + key + ", " + value + ") is in wrong bin: " + bin)
        }
      }
    }
    println()
  }
}