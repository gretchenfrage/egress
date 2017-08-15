package com.phoenixkahlo.hellcraft.carbonite.egress

import com.phoenixkahlo.hellcraft.carbonite.CarboniteOutputDebugger
import com.phoenixkahlo.hellcraft.core.{Air, BlockGrid, Chunk}
import com.phoenixkahlo.hellcraft.math.V3I


object EgressCarboniteTest extends App {

  val obj = new Chunk(V3I(0, 0, 0), BlockGrid(Air))

  val out = new CarboniteOutputDebugger(EgressCarboniteConfig)
  out.writeObject(obj)

  println()
  println(out.toArray.toSeq)

  val in = out.asInput
  val des = in.readObject()

  println()
  println("equality: " + (obj == des))
  println("before: " + obj)
  println("_after: " + des)


}
