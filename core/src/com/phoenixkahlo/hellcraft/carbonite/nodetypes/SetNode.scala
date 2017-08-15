package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.mutable.ArrayBuffer

object SetNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case set: Set[_] =>
        val boxed = set.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            boxed

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(set.size)
            for (o <- boxed)
              out.writeRef(refs(o))
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      var content: Set[_] = _

      override def read(in: CarboniteInput): Unit = {
        for (_ <- 1 to in.readInt())
          contentRefs += in.readRef()
      }

      override def get: Any = {
        if (content == null) throw new IllegalStateException
        content
      }

      override def finish(refs: (Int) => Any): Unit = {
        content = contentRefs.map(refs).toSet
      }
    }
  }

}