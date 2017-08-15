package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.mutable.ArrayBuffer

object MapNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case map: Map[_, _] =>
        val boxed = map.toSeq.map({ case (key, value) => (key.asInstanceOf[AnyRef], value.asInstanceOf[AnyRef]) })
        Some(new SerialNode {
          override val dependencies: Seq[Object] =
            boxed.flatMap(_.productIterator).map(_.asInstanceOf[AnyRef])

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(map.size)
            for ((key, value) <- boxed) {
              out.writeRef(refs(key))
              out.writeRef(refs(value))
            }
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      val contentRefs = new ArrayBuffer[(Int, Int)]
      var content: Map[_, _] = _

      override def read(in: CarboniteInput): Unit = {
        for (_ <- 1 to in.readInt())
          contentRefs += ((in.readRef(), in.readRef()))
      }

      override def get: Any = {
        if (content == null) throw new IllegalStateException
        content
      }

      override def finish(refs: (Int) => Any): Unit = {
        content = contentRefs.map({ case (key, value) => (refs(key), refs(value)) }).toMap
      }
    }
  }

}