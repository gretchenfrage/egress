package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

class StringNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case str: String =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeUTF(str)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var str: String = _

      override def read(in: CarboniteInput): Unit = {
        str = in.readUTF()
      }

      override def get: Any = str

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}
