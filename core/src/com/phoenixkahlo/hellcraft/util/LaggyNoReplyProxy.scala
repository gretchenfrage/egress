package com.phoenixkahlo.hellcraft.util

import java.lang

import scala.concurrent.duration.Duration
import java.lang.reflect
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

object LaggyNoReplyProxy {

  val scheduler = new ScheduledThreadPoolExecutor(1, runnable => new Thread(runnable, "LaggyNoReplyProxy executor thread"))

  def apply[T](source: T, lag: Duration, primaryType: Class[T], additionalTypes: Class[_]*): T = {
    reflect.Proxy.newProxyInstance(ClassLoader.getSystemClassLoader, (primaryType +: additionalTypes).toArray,
      (proxy, method, args) => {
        scheduler.schedule(() => {
          method.invoke(source, args: _*)
        }, lag toMillis, TimeUnit.MILLISECONDS)
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
