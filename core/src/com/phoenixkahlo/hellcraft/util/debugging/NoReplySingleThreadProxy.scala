package com.phoenixkahlo.hellcraft.util.debugging

import java.lang
import java.lang.reflect
import java.util.concurrent.Executors

object NoReplySingleThreadProxy {

  def apply[T](source: T, threadName: String, primaryType: Class[T], additionalTypes: Class[_]*): T = {
    val executor = Executors.newSingleThreadExecutor(runnable => new Thread(runnable, threadName))
    reflect.Proxy.newProxyInstance(ClassLoader.getSystemClassLoader, (primaryType +: additionalTypes).toArray,
      (proxy, method, args) => {
        executor.execute(() => {
          method.invoke(source, args)
        })
        method.getReturnType.getName match {
          case "int" => new lang.Integer(0)
          case "long" => new lang.Long(0)
          case "float" => new lang.Float(0)
          case "double" => new lang.Double(0)
          case "byte" => new lang.Byte(0.asInstanceOf[Byte])
          case "char" => new lang.Character(0)
          case "short" => new lang.Short(0.asInstanceOf[Short])
          case "boolean" => lang.Boolean.FALSE
          case _ => null
        }
      }
    ).asInstanceOf[T]
  }

}
