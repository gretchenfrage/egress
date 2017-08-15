package com.phoenixkahlo.hellcraft.carbonite

import java.lang.reflect.{Field, Method, Modifier}

object ReflectUtil {

  def isTransient(field: Field): Boolean = {
    Modifier.isTransient(field.getModifiers)
  }

  def serializableFields(clazz: Class[_]): Seq[Field] = {
    if (clazz == classOf[Object]) Seq.empty
    else clazz.getDeclaredFields.toSeq.filterNot(isTransient) ++ serializableFields(clazz.getSuperclass)
  }

  val modsField = classOf[Field].getDeclaredField("modifiers")
  modsField.setAccessible(true)

  def makeSettable(field: Field): Field = {
    field.setAccessible(true)
    modsField.setInt(field, field.getModifiers & ~Modifier.FINAL)
    field
  }

  def getMethodByName(clazz: Class[_], name: String): Method = {
    if (clazz == classOf[Object]) ???
    else clazz.getDeclaredMethods.toSeq.find(_.getName == name) match {
      case Some(method) => method
      case None => getMethodByName(clazz.getSuperclass, name)
    }
  }


}