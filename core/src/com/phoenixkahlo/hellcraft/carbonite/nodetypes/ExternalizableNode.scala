package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import java.io.Externalizable

import com.phoenixkahlo.hellcraft.carbonite._

class ExternalizableNode(clazz: Class[_ <: Externalizable]) extends NodeType {

  private val instantiator = GlobalObjenesis.getInstantiatorOf(clazz)

  override def serial(obj: Any): Option[SerialNode] = {
    if (obj.getClass == clazz) {
      Some(new SerialNode {
        override def dependencies: Seq[Object] = Seq.empty

        override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
          obj.asInstanceOf[Externalizable].writeExternal(out)
        }
      })
    } else None
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var content: Externalizable = _

      override def read(in: CarboniteInput): Unit = {
        content = instantiator.newInstance()
        content.readExternal(in)
      }

      override def get: Any = content

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}
