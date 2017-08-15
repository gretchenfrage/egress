package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import java.io.{ObjectInputStream, ObjectOutputStream}

import com.phoenixkahlo.hellcraft.carbonite._

class JavaSerialNode(clazz: Class[_ <: Serializable]) extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    if (clazz.isAssignableFrom(obj.getClass)) {
      Some(new SerialNode {
        override def dependencies: Seq[Object] = Seq.empty

        override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
          val jout = new ObjectOutputStream(DataIOAsStream(out))
          jout.writeObject(obj)
        }
      })
    } else None
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var contents: Any = _

      override def read(in: CarboniteInput): Unit = {
        val jin = new ObjectInputStream(DataIOAsStream(in))
        contents = jin.readObject()
      }

      override def get: Any = contents

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}
