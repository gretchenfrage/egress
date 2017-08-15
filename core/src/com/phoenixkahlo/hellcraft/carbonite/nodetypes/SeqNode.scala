package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.mutable.ArrayBuffer

object SeqNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case seq: Seq[_] =>
        val boxed = seq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            boxed

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(seq.size)
            boxed.foreach(o => out.writeRef(refs(o)))
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val n = ???

      override def read(in: CarboniteInput): Unit = {

      }

      override def get: Any = ???

      override def finish(refs: (Int) => Any): Unit = ???
    }
  }

}