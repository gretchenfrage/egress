package com.phoenixkahlo.hellcraft.carbonite

trait NodeType {

  def serial(obj: Any): Option[SerialNode]

  def deserial(): DeserialNode

}
