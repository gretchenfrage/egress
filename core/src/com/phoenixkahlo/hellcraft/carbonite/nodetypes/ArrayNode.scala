package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._
import java.lang.reflect

import scala.collection.mutable.ArrayBuffer

class RefArrayNode(clazz: Class[Array[_ <: AnyRef]]) extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    if (obj.getClass == clazz) {
      val length = reflect.Array.getLength(obj)
      val contents = (0 until length).map(reflect.Array.get(obj, _))
      Some(new SerialNode {
        override def dependencies: Seq[Object] =
          contents

        override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
          out.writeInt(length)
          contents.foreach(o => out.writeRef(refs(o)))
        }
      })
    } else None
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      var arr: AnyRef = _

      override def read(in: CarboniteInput): Unit = {
        for (_ <- 1 to in.readInt())
          contentRefs += in.readRef()
        arr = reflect.Array.newInstance(clazz.getComponentType, contentRefs.size)
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = {
        for ((ref, i) <- contentRefs.zipWithIndex) {
          reflect.Array.set(arr, i, refs(contentRefs(i)))
        }
      }
    }
  }

}

object DoubleArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Double] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeDouble(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Double] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Double](length)
        for (i <- 0 until length)
          arr(i) = in.readDouble()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object FloatArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Float] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeFloat(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Float] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Float](length)
        for (i <- 0 until length)
          arr(i) = in.readFloat()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object IntArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Int] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeInt(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Int] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Int](length)
        for (i <- 0 until length)
          arr(i) = in.readInt()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object LongArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Long] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeLong(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Long] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Long](length)
        for (i <- 0 until length)
          arr(i) = in.readLong()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object ShortArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Short] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeShort(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Short] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Short](length)
        for (i <- 0 until length)
          arr(i) = in.readShort()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object ByteArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Byte] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeByte(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Byte] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Byte](length)
        for (i <- 0 until length)
          arr(i) = in.readByte()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object CharArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Char] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeChar(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Char] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Char](length)
        for (i <- 0 until length)
          arr(i) = in.readChar()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}

object BooleanArrayNode extends NodeType {
  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case arr: Array[Boolean] =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Seq.empty

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = {
            out.writeInt(arr.length)
            for (n <- arr)
              out.writeBoolean(n)
          }
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var arr: Array[Boolean] = _

      override def read(in: CarboniteInput): Unit = {
        val length = in.readInt()
        arr = new Array[Boolean](length)
        for (i <- 0 until length)
          arr(i) = in.readBoolean()
      }

      override def get: Any = arr

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }
}