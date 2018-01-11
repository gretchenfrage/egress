package com.phoenixkahlo.hellcraft.service

class ServiceTagTable[V[_ <: Service] <: AnyRef] {
  private val contents = new Array[AnyRef](ServiceTag.total)

  def +=[S <: Service](v: V[S])(implicit tag: ServiceTag[S]): Unit =
    contents(tag.ord) = v

  def apply[S <: Service](implicit k: ServiceTag[S]): V[S] =
    contents(k.ord).asInstanceOf[V[S]]

  def toSeq: Seq[V[_ <: Service]] = contents.toSeq.asInstanceOf[Seq[V[_ <: Service]]]
}
