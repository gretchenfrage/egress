package com.phoenixkahlo.hellcraft.carbonite

import java.io.Externalizable
import java.util.UUID

import com.phoenixkahlo.hellcraft.carbonite.nodetypes._

import scala.collection.{SortedMap, SortedSet, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait CarboniteConfig {

  def serial(obj: Any): (SerialNode, NodeTypeID)

  def deserial(id: NodeTypeID): DeserialNode

  def classes: Seq[Class[_]]

}

class DefaultCarboniteConfig extends CarboniteConfig {

  override val classes = new ArrayBuffer[Class[_]]
  val types = new ArrayBuffer[NodeType]

  def register(clazz: Class[_]): Unit = {
    classes += clazz

    val nodeType = resolveType(clazz)
    if (!types.contains(nodeType))
      types += nodeType
  }

  def register[T]()(implicit tag: ClassTag[T]): Unit = {
    register(tag.runtimeClass)
  }

  override def serial(obj: Any): (SerialNode, NodeTypeID) = {
    try {
      types.zipWithIndex.flatMap({ case (nType, id) => nType.serial(obj).map((_, id.toByte)) }).head
    } catch {
      case e: NoSuchElementException =>
        System.err.println("no valid serializer for " + obj)
        throw e
    }
  }

  override def deserial(id: NodeTypeID): DeserialNode = {
    try {
      types(id).deserial()
    } catch {
      case e: NoSuchElementException =>
        System.err.println("no registered deserializer for " + id)
        throw e
    }
  }

  protected def resolveType(clazz: Class[_]): NodeType = {
    // detect annotation
    if (clazz.isAnnotationPresent(classOf[CarboniteWith])) {
      clazz.getAnnotation(classOf[CarboniteWith]).value.asInstanceOf[Class[_ <: NodeType]]
        .getConstructor(classOf[Class[_]]).newInstance(clazz)
    }
    // externalizable
    else if (clazz.getInterfaces.contains(classOf[Externalizable]))
      new ExternalizableNode(clazz.asInstanceOf[Class[Externalizable]])
    // string
    else if (clazz == classOf[String]) StringNode
    // collections
    else if (clazz == classOf[Seq[_]]) SeqNode
    else if (clazz == classOf[Map[_, _]]) MapNode
    else if (clazz == classOf[Set[_]]) SetNode
    else if (clazz == classOf[SortedMap[_, _]]) SortedMapNode
    else if (clazz == classOf[SortedSet[_]]) SortedSetNode
    // singletons
    else if (ReflectUtil.isSingleton(clazz)) SingletonNode
    // arrays
    else if (clazz.isArray) {
      val comp = clazz.getComponentType
      if (comp.isPrimitive) {
        if (comp == classOf[Int] || comp == classOf[java.lang.Integer]) IntArrayNode
        else if (comp == classOf[Long] || comp == classOf[java.lang.Long]) LongArrayNode
        else if (comp == classOf[Short] || comp == classOf[java.lang.Short]) ShortArrayNode
        else if (comp == classOf[Byte] || comp == classOf[java.lang.Byte]) ByteArrayNode
        else if (comp == classOf[Char] || comp == classOf[java.lang.Character]) CharArrayNode
        else if (comp == classOf[Boolean] || comp == classOf[java.lang.Boolean]) BooleanArrayNode
        else if (comp == classOf[Float] || comp == classOf[java.lang.Float]) DoubleArrayNode
        else if (comp == classOf[Double] || comp == classOf[java.lang.Double]) FloatArrayNode
        else ???
      } else new RefArrayNode(clazz.asInstanceOf[Class[Array[_ <: AnyRef]]])
    }
    // tuples
    else if (clazz == classOf[Tuple1[_]]) Tuple1Node
    else if (clazz == classOf[Tuple2[_, _]]) Tuple2Node
    else if (clazz == classOf[Tuple3[_, _, _]]) Tuple3Node
    else if (clazz == classOf[Tuple4[_, _, _, _]]) Tuple4Node
    else if (clazz == classOf[Tuple5[_, _, _, _, _]]) Tuple5Node
    else if (clazz == classOf[Tuple6[_, _, _, _, _, _]]) Tuple6Node
    else if (clazz == classOf[Tuple7[_, _, _, _, _, _, _]]) Tuple7Node
    else if (clazz == classOf[Tuple8[_, _, _, _, _, _, _, _]]) Tuple8Node
    else if (clazz == classOf[Tuple9[_, _, _, _, _, _, _, _, _]]) Tuple9Node
    else if (clazz == classOf[Tuple10[_, _, _, _, _, _, _, _, _, _]]) Tuple10Node
    else if (clazz == classOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]]) Tuple11Node
    else if (clazz == classOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]]) Tuple12Node
    else if (clazz == classOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple13Node
    else if (clazz == classOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple14Node
    else if (clazz == classOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple15Node
    else if (clazz == classOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple16Node
    else if (clazz == classOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple17Node
    else if (clazz == classOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple18Node
    else if (clazz == classOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple19Node
    else if (clazz == classOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple20Node
    else if (clazz == classOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple21Node
    else if (clazz == classOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]) Tuple22Node
    // primitives and boxes
    else if (clazz == classOf[Int] || clazz == classOf[java.lang.Integer]) IntNode
    else if (clazz == classOf[Long] || clazz == classOf[java.lang.Long]) LongNode
    else if (clazz == classOf[Short] || clazz == classOf[java.lang.Short]) ShortNode
    else if (clazz == classOf[Byte] || clazz == classOf[java.lang.Byte]) ByteNode
    else if (clazz == classOf[Char] || clazz == classOf[java.lang.Character]) CharNode
    else if (clazz == classOf[Boolean] || clazz == classOf[java.lang.Boolean]) BooleanNode
    else if (clazz == classOf[Float] || clazz == classOf[java.lang.Float]) FloatNode
    else if (clazz == classOf[Double] || clazz == classOf[java.lang.Double]) DoubleNode
    // existing classes to use field serialization for
    else if (clazz == classOf[UUID]) new FieldNode(clazz)
    // things to use java serialization for
    else if (clazz == classOf[Ordering[_]]) new JavaSerialNode(classOf[Ordering[_]])
    // undefined
    else {
      System.err.println("could not resolve serializer for " + clazz)
      ???
    }
  }

  register[Seq[_]]()
  register[Map[_, _]]()
  register[Set[_]]()
  register[SortedMap[_, _]]()
  register[SortedSet[_]]()

  register[LazyDeserial[_]]()

  register[String]()

  register[Tuple1[_]]()
  register[Tuple2[_, _]]()
  register[Tuple3[_, _, _]]()
  register[Tuple4[_, _, _, _]]()
  register[Tuple5[_, _, _, _, _]]()
  register[Tuple6[_, _, _, _, _, _]]()
  register[Tuple7[_, _, _, _, _, _, _]]()
  register[Tuple8[_, _, _, _, _, _, _, _]]()
  register[Tuple9[_, _, _, _, _, _, _, _, _]]()
  register[Tuple10[_, _, _, _, _, _, _, _, _, _]]()
  register[Tuple11[_, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()
  register[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]()

  register[Int]()
  register[Long]()
  register[Short]()
  register[Byte]()
  register[Char]()
  register[Boolean]()
  register[Float]()
  register[Double]()

  register[UUID]()

  register[Ordering[_]]()

}