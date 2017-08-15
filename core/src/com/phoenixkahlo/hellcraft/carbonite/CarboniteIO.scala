package com.phoenixkahlo.hellcraft.carbonite

import java.io._
import java.util

import scala.collection.{JavaConverters, mutable}
import scala.concurrent.JavaConversions

trait CarboniteOutput extends ObjectOutput {

  def writeRefCount(count: Int): Unit

  def writeRef(n: Int): Unit

  def writeClass(clazz: Class[_]): Unit

  protected def config: CarboniteConfig

  override def writeObject(obj: Any): Unit = {
    case class Datum(node: SerialNode, ref: Int, nodeTypeID: NodeTypeID)
    var refCount = 0
    val data = new util.IdentityHashMap[Any, Datum]

    def proc(obj: Any): Unit = {
      if (!data.containsKey(obj)) {
        val (node, typeID) = config.serial(obj)
        data.put(obj, Datum(node, refCount, typeID))
        refCount += 1

        for (dep <- node.dependencies.filter(_ != null))
          proc(dep)
      }
    }
    proc(obj)

    var toWrite = new mutable.TreeSet[Datum]()(Ordering.by(_.ref))
    for (datum <- JavaConverters.collectionAsScalaIterable(data.values()))
      toWrite += datum

    val refLookup: Any => Int = obj =>
      if (obj == null) -1
      else data.get(obj).ref

    writeRefCount(refCount)
    for (datum <- toWrite) {
      writeByte(datum.nodeTypeID)
      datum.node.write(this, refLookup)
    }
  }

}

trait CarboniteInput extends ObjectInput {

  def readRefCount(): Int

  def readRef(): Int

  def readClass(): Class[_]

  protected def config: CarboniteConfig

  override def readObject(): AnyRef = {
    val refCount = readRefCount()
    val refs = new mutable.HashMap[Int, DeserialNode]
    for (ref <- 0 until refCount) {
      val typeID = readByte()
      val node = config.deserial(typeID)
      node.read(this)
      refs.put(ref, node)
    }
    val refLookup: Int => Any = n =>
      if (n == -1) null
      else refs(n).get
    for (node <- refs.values) {
      node.finish(refLookup)
    }
    refs(0).get.asInstanceOf[AnyRef]
  }

}

sealed trait DatumType
object ByteDatum extends DatumType
object ShortDatum extends DatumType
object IntDatum extends DatumType

class CarboniteOutputStream(out: OutputStream)(implicit override val config: CarboniteConfig)
  extends DataOutputStream(out) with CarboniteOutput {

  private var refType: DatumType = _
  private val classMap = config.classes.zip(config.classes.indices).toMap
  private val classType: DatumType = config.classes.size match {
    case n if n <= Byte.MaxValue => ByteDatum
    case n if n <= Short.MaxValue => ShortDatum
    case _ => IntDatum
  }

  override def writeRefCount(count: Int): Unit = {
    if (count <= Byte.MaxValue) refType = ByteDatum
    else if (count <= Short.MaxValue) refType = ShortDatum
    else refType = IntDatum
    writeInt(count)
  }

  override def writeRef(n: Int): Unit = {
    refType match {
      case ByteDatum => writeByte(n)
      case ShortDatum => writeShort(n)
      case IntDatum => writeInt(n)
    }
  }

  override def writeClass(clazz: Class[_]): Unit = {
    val n = classMap(clazz)
    classType match {
      case ByteDatum => writeByte(n)
      case ShortDatum => writeShort(n)
      case IntDatum => writeInt(n)
    }
  }

}

class CarboniteInputStream(in: InputStream)(implicit override val config: CarboniteConfig)
  extends DataInputStream(in) with CarboniteInput {

  private var refType: DatumType = _
  private val classMap = config.classes.indices.zip(config.classes).toMap
  private var classType: DatumType = config.classes.size match {
    case n if n <= Byte.MaxValue => ByteDatum
    case n if n <= Short.MaxValue => ShortDatum
    case _ => IntDatum
  }

  override def readRefCount(): Int = {
    val count = readInt()
    if (count <= Byte.MaxValue) refType = ByteDatum
    else if (count <= Short.MaxValue) refType = ShortDatum
    else refType = IntDatum
    count
  }

  override def readRef(): Int = {
    refType match {
      case ByteDatum => readByte()
      case ShortDatum => readShort()
      case IntDatum => readInt()
    }
  }

  override def readClass(): Class[_] = {
    val n = classType match {
      case ByteDatum => readByte()
      case ShortDatum => readShort()
      case IntDatum => readInt()
    }
    classMap(n)
  }

}

class CarboniteOutputDebugger(override val config: CarboniteConfig) extends CarboniteOutput {

  private val baos = new ByteArrayOutputStream
  val out = new CarboniteOutputStream(baos)(config)

  def toArray: Array[Byte] = {
    baos.toByteArray
  }

  def asInput: CarboniteInput = {
    new CarboniteInputStream(new ByteArrayInputStream(baos.toByteArray))(config)
  }

  override def writeRefCount(count: Int): Unit = {
    println("ref count: " + count)
    out.writeRefCount(count)
  }

  override def writeRef(n: Int): Unit = {
    println("ref: " + n)
    out.writeRef(n)
  }

  override def writeClass(clazz: Class[_]): Unit = {
    println("class: " + clazz.getSimpleName)
    out.writeClass(clazz)
  }

  override def flush(): Unit = out.flush()

  override def close(): Unit = out.close()

  override def write(b: Int): Unit = {
    println("byte: " + b)
    out.write(b)
  }

  override def write(b: Array[Byte]): Unit = {
    println("arr: " + util.Arrays.toString(b))
    out.write(b)
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    println("arr: " + b + " offset " + off + " len " + len)
    out.write(b, off, len)
  }

  override def writeFloat(v: Float): Unit = {
    println("float: " + v)
    out.writeFloat(v)
  }

  override def writeLong(v: Long): Unit = {
    println("long: " + v)
    out.writeLong(v)
  }

  override def writeDouble(v: Double): Unit = {
    println("double: " + v)
    out.writeDouble(v)
  }

  override def writeInt(v: Int): Unit = {
    println("int: " + v)
    out.writeInt(v)
  }

  override def writeByte(v: Int): Unit = {
    println("byte: " + v)
    out.writeByte(v)
  }

  override def writeChar(v: Int): Unit = {
    println("chars: " + v)
    out.writeChar(v)
  }

  override def writeBytes(s: String): Unit = {
    println("bytes: " + s)
    out.writeBytes(s)
  }

  override def writeUTF(s: String): Unit = {
    println("utf: " + s)
    out.writeUTF(s)
  }

  override def writeShort(v: Int): Unit = {
    println("short: " + v)
    out.writeShort(v)
  }

  override def writeChars(s: String): Unit = {
    println("chars: " + s)
    out.writeChars(s)
  }

  override def writeBoolean(v: Boolean): Unit = {
    println("boolean: " + v)
    out.writeBoolean(v)
  }
}