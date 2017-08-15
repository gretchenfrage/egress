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
      val content = new SeqDelegate[Any]

      override def read(in: CarboniteInput): Unit = {
        for (_ <- 1 to in.readInt())
          contentRefs += in.readRef()
      }

      override def get: Any =
        content

      override def finish(refs: (Int) => Any): Unit = {
        content.source = contentRefs.foldLeft(Vector[Any]())((v, r) => v :+ refs(r))
      }
    }
  }

}