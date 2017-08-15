package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import java.io.{File, FileOutputStream, PrintStream}

import com.phoenixkahlo.hellcraft.carbonite._
import other.AppDirs

import scala.collection.mutable.ArrayBuffer

object TupleNodeSourceFactory extends App {

  /*
  val classFile = new File(System.getProperty("user.home"), "tupleClasses.txt")
  val classFileOut = new PrintStream(new FileOutputStream(classFile))
  System.setOut(classFileOut)
  */

  for (n <- 3 to 22) {

    val className = "Tuple" + n + "[" + (1 to n).foldLeft("")((str, _) => str + "_, ").substring(0, n * 3 - 2) + "]"
    println("class Tuple" + n + "Node extends NodeType {")
    println()
    println("  private val clazz = classOf[" + className + "]")
    println("  private val fields = (1 to " + n + ").map(n => clazz.getDeclaredField(\"_\" + n)).map(ReflectUtil.makeSettable)")
    println()
    println("  override def serial(obj: Any): Option[SerialNode] =")
    println("    obj match {")
    println("      case tup: " + className + " =>")
    println("        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])")
    println("        Some(new SerialNode {")
    println("          override def dependencies: Seq[Object] = ")
    println("            contents")
    println()
    println("          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit = ")
    println("            contents.foreach(o => out.writeRef(refs(o)))")
    println("        })")
    println("      case _ => None")
    println("    }")
    println()
    println("  override def deserial(): DeserialNode = ")
    println("    new DeserialNode {")
    println("      val contentRefs = new ArrayBuffer[Int]")
    println("      val tup = Tuple" + n + "(" + (1 to n).foldLeft("")((str, _) => str + "null, ").substring(0, n * 6 - 2) + ")")
    println()
    println("      override def read(in: CarboniteInput): Unit = ")
    println("        for (_ <- 1 to " + n + ") contentRefs += in.readRef()")
    println()
    println("      override def get: Any = ")
    println("        tup")
    println()
    println("      override def finish(refs: (Int) => Any): Unit = ")
    println("        for (i <- 0 until " + n + ") {")
    println("          fields(i).set(tup, refs(contentRefs(i)))")
    println("        }")
    println("    }")
    println()
    println("}")
  }

  for (n <- 1 to 22) {
    val className = "Tuple" + n + "[" + (1 to n).foldLeft("")((str, _) => str + "_, ").substring(0, n * 3 - 2) + "]"
    println("else if (clazz == classOf[" + className + "]) new Tuple" + n + "Node")
  }

  println()

  for (n <- 1 to 22) {
    val className = "Tuple" + n + "[" + (1 to n).foldLeft("")((str, _) => str + "_, ").substring(0, n * 3 - 2) + "]"
    println("register[" + className + "]()")
  }

}

class Tuple1Node extends NodeType {

  private val clazz = classOf[Tuple1[_]]
  private val fields = (1 to 1).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple1[_] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple1(null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 1) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 1) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}

class Tuple2Node extends NodeType {

  private val clazz = classOf[Tuple2[_, _]]
  private val fields = (1 to 2).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple2[_, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple2(null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 2) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 2) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}

class Tuple3Node extends NodeType {

  private val clazz = classOf[Tuple3[_, _, _]]
  private val fields = (1 to 3).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple3[_, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple3(null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 3) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 3) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple4Node extends NodeType {

  private val clazz = classOf[Tuple4[_, _, _, _]]
  private val fields = (1 to 4).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple4[_, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple4(null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 4) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 4) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple5Node extends NodeType {

  private val clazz = classOf[Tuple5[_, _, _, _, _]]
  private val fields = (1 to 5).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple5[_, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple5(null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 5) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 5) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple6Node extends NodeType {

  private val clazz = classOf[Tuple6[_, _, _, _, _, _]]
  private val fields = (1 to 6).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple6[_, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple6(null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 6) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 6) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple7Node extends NodeType {

  private val clazz = classOf[Tuple7[_, _, _, _, _, _, _]]
  private val fields = (1 to 7).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple7[_, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple7(null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 7) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 7) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple8Node extends NodeType {

  private val clazz = classOf[Tuple8[_, _, _, _, _, _, _, _]]
  private val fields = (1 to 8).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple8[_, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple8(null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 8) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 8) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple9Node extends NodeType {

  private val clazz = classOf[Tuple9[_, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 9).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple9[_, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple9(null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 9) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 9) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple10Node extends NodeType {

  private val clazz = classOf[Tuple10[_, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 10).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple10[_, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple10(null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 10) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 10) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple11Node extends NodeType {

  private val clazz = classOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 11).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple11[_, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple11(null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 11) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 11) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple12Node extends NodeType {

  private val clazz = classOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 12).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple12[_, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple12(null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 12) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 12) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple13Node extends NodeType {

  private val clazz = classOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 13).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple13(null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 13) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 13) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple14Node extends NodeType {

  private val clazz = classOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 14).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple14(null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 14) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 14) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple15Node extends NodeType {

  private val clazz = classOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 15).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple15(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 15) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 15) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple16Node extends NodeType {

  private val clazz = classOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 16).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple16(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 16) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 16) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple17Node extends NodeType {

  private val clazz = classOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 17).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple17(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 17) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 17) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple18Node extends NodeType {

  private val clazz = classOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 18).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple18(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 18) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 18) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple19Node extends NodeType {

  private val clazz = classOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 19).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple19(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 19) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 19) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple20Node extends NodeType {

  private val clazz = classOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 20).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple20(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 20) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 20) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple21Node extends NodeType {

  private val clazz = classOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 21).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple21(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 21) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 21) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
class Tuple22Node extends NodeType {

  private val clazz = classOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  private val fields = (1 to 22).map(n => clazz.getDeclaredField("_" + n)).map(ReflectUtil.makeSettable)

  override def serial(obj: Any): Option[SerialNode] =
    obj match {
      case tup: Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val contents = tup.productIterator.toSeq.map(_.asInstanceOf[AnyRef])
        Some(new SerialNode {
          override def dependencies: Seq[Object] =
            contents

          override def write(out: CarboniteOutput, refs: (Any) => Int): Unit =
            contents.foreach(o => out.writeRef(refs(o)))
        })
      case _ => None
    }

  override def deserial(): DeserialNode =
    new DeserialNode {
      val contentRefs = new ArrayBuffer[Int]
      val tup = Tuple22(null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null)

      override def read(in: CarboniteInput): Unit =
        for (_ <- 1 to 22) contentRefs += in.readRef()

      override def get: Any =
        tup

      override def finish(refs: (Int) => Any): Unit =
        for (i <- 0 until 22) {
          fields(i).set(tup, refs(contentRefs(i)))
        }
    }

}
