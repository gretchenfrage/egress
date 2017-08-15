package com.phoenixkahlo.hellcraft.carbonite

trait SerialNode {

  def dependencies: Seq[Object]

  def write(out: CarboniteOutput, refs: Any => Int): Unit

}

trait DeserialNode {

  def read(in: CarboniteInput): Unit

  def get: Any

  def finish(refs: Int => Any): Unit

}

