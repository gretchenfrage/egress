package com.phoenixkahlo.hellcraft.math

sealed trait Facet

case class Tri(a: V3F, b: V3F, c: V3F) extends Facet {

  def bothSides: Seq[Tri] =
    Seq(this, Tri(c, b, a))

}

case class Quad(a: V3F, b: V3F, c: V3F, d: V3F) extends Facet {

  def decompose: Seq[Tri] =
    Seq(Tri(a, b, c), Tri(a, c, d))

}