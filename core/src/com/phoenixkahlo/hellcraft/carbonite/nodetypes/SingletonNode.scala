package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

object SingletonNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    if (ReflectUtil.isSingleton(obj.getClass)) {
      Some(new SerialNode {
        override def dependencies: Seq[Object] =
          Seq.empty

        override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
          out.writeClass(obj.getClass)
      })
    } else None
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var obj: AnyRef = _

      override def read(in: CarboniteInput): Unit = {
        val clazz = in.readClass()
        obj = clazz.getField("MODULE$").get(obj)
      }

      override def get: Any = obj

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}
