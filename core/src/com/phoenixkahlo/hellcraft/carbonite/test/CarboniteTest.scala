package com.phoenixkahlo.hellcraft.carbonite.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util

import com.phoenixkahlo.hellcraft.carbonite._
import com.phoenixkahlo.hellcraft.carbonite.nodetypes._

import scala.collection.{SortedMap, SortedSet}

@CarboniteWith(classOf[FieldNode])
class NormalClass(val a: String, val b: Int, val c: Double) {

  val d: Long = 42
  @transient val e: Short = 2

}

@CarboniteWith(classOf[FieldNode])
case class CaseClass(a: Int, b: Int, c: Any)

object SingletonObject

object CarboniteTest extends App {

  val obj = SortedMap(
    1 -> "a",
    2 -> "b",
    3 -> "c"
  )

  val out = new CarboniteOutputDebugger(new DefaultCarboniteConfig)
  out.writeObject(obj)

  val in = out.asInput
  val des = in.readObject()

  println()
  println("equality: " + (obj == des))
  println("before: " + obj)
  println("after: " + des)

}