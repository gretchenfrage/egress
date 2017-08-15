package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

object IntNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Int =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeInt(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Int = _

      override def read(in: CarboniteInput): Unit = n = in.readInt()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object LongNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Long =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeLong(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Long = _

      override def read(in: CarboniteInput): Unit = n = in.readLong()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object DoubleNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Double =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeDouble(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Double = _

      override def read(in: CarboniteInput): Unit = n = in.readDouble()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object FloatNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Float =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeFloat(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Float = _

      override def read(in: CarboniteInput): Unit = n = in.readFloat()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object ByteNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Byte =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeByte(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Byte = _

      override def read(in: CarboniteInput): Unit = n = in.readByte()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object ShortNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Short =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeShort(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Short = _

      override def read(in: CarboniteInput): Unit = n = in.readShort()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object BooleanNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Boolean =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeBoolean(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Boolean = _

      override def read(in: CarboniteInput): Unit = n = in.readBoolean()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}

object CharNode extends NodeType {

  override def serial(obj: Any): Option[SerialNode] = {
    obj match {
      case n: Char =>
        Some(new SerialNode {
          override def dependencies: Seq[Object] = Nil

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = out.writeChar(n)
        })
      case _ => None
    }
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      var n: Char = _

      override def read(in: CarboniteInput): Unit = n = in.readChar()

      override def get: Any = n

      override def finish(refs: (Int) => Any): Unit = ()
    }
  }

}
