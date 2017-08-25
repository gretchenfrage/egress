package com.phoenixkahlo.hellcraft.math

import com.phoenixkahlo.hellcraft.carbonite.CarboniteWith
import com.phoenixkahlo.hellcraft.carbonite.nodetypes.FieldNode

sealed trait Facet

@CarboniteWith(classOf[FieldNode])
case class Tri(a: V3F, b: V3F, c: V3F) extends Facet {

  def bothSides: Seq[Tri] =
    Seq(this, Tri(c, b, a))

}

@CarboniteWith(classOf[FieldNode])
case class Quad(a: V3F, b: V3F, c: V3F, d: V3F) extends Facet {

  def decompose: Seq[Tri] =
    Seq(Tri(a, b, c), Tri(a, c, d))

  def map(func: V3F => V3F): Quad =
    Quad(func(a), func(b), func(c), func(d))

}