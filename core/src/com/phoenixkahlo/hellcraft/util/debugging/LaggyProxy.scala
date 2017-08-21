package com.phoenixkahlo.hellcraft.util.debugging

import java.lang.reflect
import java.lang.reflect.InvocationTargetException

import scala.concurrent.duration.Duration

/**
  * Wrap your remote objects with these to simulate lag
  */
object LaggyProxy {

  def apply[T](source: T, lag: Duration, primaryType: Class[T], additionalTypes: Class[_]*): T = {
    reflect.Proxy.newProxyInstance(ClassLoader.getSystemClassLoader, (primaryType +: additionalTypes).toArray,
      (proxy, method, args) => {
        try {
          Thread.sleep(lag toMillis)
          method.invoke(source, args: _*)
        } catch {
          case ite: InvocationTargetException => throw ite.getTargetException
        } finally {
          Thread.sleep(lag toMillis)
        }
      }).asInstanceOf[T]
  }

}