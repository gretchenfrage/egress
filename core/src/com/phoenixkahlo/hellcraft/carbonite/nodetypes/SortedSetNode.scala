package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer

object SortedSetNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case set: SortedSet[_] =>
        val boxed = set.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            boxed :+ set.ordering

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(set.size)
            out.writeRef(refs(set.ordering))
            for (o <- boxed)
              out.writeRef(refs(o))
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var orderingRef: Int = _
      val contentRefs = new ArrayBuffer[Int]
      val content = new SortedSetDelegate[Any]

      override def read(in: CarboniteInput): Unit = {
        val size = in.readInt()
        orderingRef = in.readRef()
        for (_ <- 1 to size)
          contentRefs += in.readInt()
      }

      override def get: Any =
        content

      override def finish(refs: (Int) => Any): Unit = {
        val ord = refs(orderingRef).asInstanceOf[Ordering[Any]]
        content.source = contentRefs.foldLeft(SortedSet.empty[Any](ord))(_ + _)
      }
    }
  }

}
