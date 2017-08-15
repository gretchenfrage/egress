package com.phoenixkahlo.hellcraft.carbonite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait CarboniteConfig {

  def serial(obj: Any): (SerialNode, NodeTypeID)

  def deserial(id: NodeTypeID): DeserialNode

  def classes: Seq[Class[_]]

}

abstract class AbstractCarboniteConfig extends CarboniteConfig {

  override val classes = new ArrayBuffer[Class[_]]
  val types = new ArrayBuffer[NodeType]

  protected def resolveType(clazz: Class[_]): NodeType

  def register(clazz: Class[_]): Unit = {
    classes += clazz

    val nodeType = Option(clazz.getAnnotation(classOf[CarboniteWith])) match {
      case Some(carboniteWith) => carboniteWith.value.asInstanceOf[Class[_ <: NodeType]]
        .getConstructor(classOf[Class[_]]).newInstance(clazz)
      case None => resolveType(clazz)
    }
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

}