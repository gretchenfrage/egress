package com.phoenixkahlo.hellcraft.math

sealed trait Facet

case class Tri(a: V3F, b: V3F, c: V3F) extends Facet

case class Quad(a: V3F, b: V3F, c: V3F, d: V3F) extends Facet
