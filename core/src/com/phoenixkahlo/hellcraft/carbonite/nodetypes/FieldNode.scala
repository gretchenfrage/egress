package com.phoenixkahlo.hellcraft.carbonite.nodetypes

import com.phoenixkahlo.hellcraft.carbonite._

import scala.collection.mutable.ArrayBuffer

class FieldNode(clazz: Class[_]) extends NodeType {

  private val fields = ReflectUtil.serializableFields(clazz).map(ReflectUtil.makeSettable)
  private val instantiator = GlobalObjenesis.getInstantiatorOf(clazz)

  override def serial(obj: Any): Option[SerialNode] = {
    if (obj.getClass == clazz) {
      Some(new SerialNode {
        override def dependencies: Seq[Object] = {
          fields.filter(!_.getType.isPrimitive).map(_.get(obj))
        }

        override def write(out: CarboniteOutput, refs: Any => Int): Unit = {
          for (field <- fields) {
            val fieldType = field.getType
            if (fieldType.isPrimitive) {
              if (fieldType == classOf[Int]) out.writeInt(field.getInt(obj))
              else if (fieldType == classOf[Byte]) out.writeByte(field.getByte(obj))
              else if (fieldType == classOf[Long]) out.writeLong(field.getLong(obj))
              else if (fieldType == classOf[Short]) out.writeShort(field.getShort(obj))
              else if (fieldType == classOf[Float]) out.writeFloat(field.getFloat(obj))
              else if (fieldType == classOf[Double]) out.writeDouble(field.getDouble(obj))
              else if (fieldType == classOf[Char]) out.writeChar(field.getChar(obj))
              else if (fieldType == classOf[Boolean]) out.writeBoolean(field.getBoolean(obj))
              else ???
            } else {
              out.writeRef(refs(field.get(obj)))
            }
          }
        }
      })
    } else None
  }

  override def deserial(): DeserialNode = {
    new DeserialNode {
      val obj: Any = instantiator.newInstance()
      val contentRefs = new ArrayBuffer[Int]

      override def read(in: CarboniteInput): Unit = {
        for (field <- fields) {
          val fieldType = field.getType
          if (fieldType.isPrimitive) {
            if (fieldType == classOf[Int]) field.setInt(obj, in.readInt())
            else if (fieldType == classOf[Byte]) field.setByte(obj, in.readByte())
            else if (fieldType == classOf[Long]) field.setLong(obj, in.readLong())
            else if (fieldType == classOf[Short]) field.setShort(obj, in.readShort())
            else if (fieldType == classOf[Float]) field.setFloat(obj, in.readFloat())
            else if (fieldType == classOf[Double]) field.setDouble(obj, in.readDouble())
            else if (fieldType == classOf[Char]) field.setChar(obj, in.readChar())
            else if (fieldType == classOf[Boolean]) field.setBoolean(obj, in.readBoolean())
            else ???
          } else {
            contentRefs += in.readRef()
          }
        }
      }

      override def get: Any = obj

      override def finish(refs: Int => Any): Unit = {
        var refIndex = 0
        for (field <- fields) {
          if (!field.getType.isPrimitive) {
            field.set(obj, refs(contentRefs(refIndex)))
            refIndex += 1
          }
        }
      }
    }
  }
}
