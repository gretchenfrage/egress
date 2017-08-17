package com.phoenixkahlo.hellcraft.carbonite

import java.io._

import com.phoenixkahlo.hellcraft.math.V3F
import com.phoenixkahlo.hellcraft.threading.Fut

import scala.concurrent.{ExecutionContext, Future}

class LazyDeserial[E](private var element: E)(implicit private var config: CarboniteConfig) extends Externalizable {

  private var bytes: Array[Byte] = _

  override def writeExternal(out: ObjectOutput): Unit = {
    if (bytes == null) {
      val baos = new ByteArrayOutputStream
      val oout = new CarboniteOutputStream(baos)
      oout.writeObject(element)
      oout.flush()
      bytes = baos.toByteArray
    }
    out.writeInt(bytes.length)
    out.write(bytes)
  }

  override def readExternal(in: ObjectInput): Unit = {
    bytes = new Array[Byte](in.readInt())
    in.read(bytes)
  }

  def get(implicit config: CarboniteConfig): E = {
    if (element == null) {
      val oin = new CarboniteInputStream(new ByteArrayInputStream(bytes))
      element = oin.readObject().asInstanceOf[E]
    }
    element
  }

  def future(implicit executor: ExecutionContext, config: CarboniteConfig): Future[E] =
    Future { get(config) }

  def fut(executor: Runnable => Unit)(implicit config: CarboniteConfig): Fut[E] =
    Fut(get(config), executor)


}

object LazyDeserial {

  def apply[E](e: E)(implicit config: CarboniteConfig): LazyDeserial[E] = new LazyDeserial(e)(config)

}