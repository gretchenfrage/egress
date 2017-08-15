package com.phoenixkahlo.hellcraft.carbonite

import java.lang.reflect.{Field, Method, Modifier}

object ReflectUtil {

  def isTransient(field: Field): Boolean = {
    Modifier.isTransient(field.getModifiers)
  }

  def isStatic(field: Field): Boolean = {
    Modifier.isStatic(field.getModifiers)
  }

  def serializableFields(clazz: Class[_]): Seq[Field] = {
    if (clazz == classOf[Object]) Seq.empty
    else clazz.getDeclaredFields.toSeq.filterNot(isTransient).filterNot(isStatic) ++
      serializableFields(clazz.getSuperclass)
  }

  val modsField: Field = classOf[Field].getDeclaredField("modifiers")
  modsField.setAccessible(true)

  def makeSettable(field: Field): Field = {
    field.setAccessible(true)
    modsField.setInt(field, field.getModifiers & ~Modifier.FINAL)
    field
  }

  def getMethodByName(clazz: Class[_], name: String): Method = {
    if (clazz == classOf[Object]) throw new NoSuchElementException
    else clazz.getDeclaredMethods.toSeq.find(_.getName == name) match {
      case Some(method) => method
      case None => getMethodByName(clazz.getSuperclass, name)
    }
  }

  def isSingleton(clazz: Class[_]): Boolean = {
    clazz.getFields.exists(_.getName == "MODULE$")
  }

}