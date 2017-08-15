package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.SortedMap
import scala.collection.mutable.ArrayBuffer

object SortedMapNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case map: SortedMap[_, _] =>
        val boxed = map.map({ case (key, value) => (key.asInstanceOf[AnyRef], value.asInstanceOf[AnyRef]) })
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            boxed.keys.toSeq ++ boxed.values :+ map.ordering

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(map.size)
            out.writeRef(refs(map.ordering))
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
      var orderingRef: Int = _
      val contentRefs = new ArrayBuffer[(Int, Int)]
      val content = new SortedMapDelegate[Any, Any]

      override def read(in: CarboniteInput): Unit = {
        val size = in.readInt()
        orderingRef = in.readRef()
        for (_ <- 1 to size)
          contentRefs += ((in.readRef(), in.readRef()))
      }

      override def get: Any =
        content

      override def finish(refs: (Int) => Any): Unit = {
        val ord = refs(orderingRef).asInstanceOf[Ordering[Any]]
        content.source =
          contentRefs.foldLeft(SortedMap.empty[Any, Any](ord))({ case (map, (key, value)) => map.updated(key, value)})
      }
    }
  }
}