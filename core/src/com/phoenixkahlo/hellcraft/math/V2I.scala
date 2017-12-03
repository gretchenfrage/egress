package com.phoenixkahlo.hellcraft.math

class V2I(val xi: Int, val yi: Int) extends V2F(xi, yi) {

  def +(o: V2I): V2I =
    V2I(xi + o.xi, yi + o.yi)

  def -(o: V2I): V2I =
    V2I(xi - o.xi, yi - o.yi)

  def *(s: Int): V2I =
    V2I(xi * s, yi * s)

  def %(s: Int): V2I =
    if (this >= V2I(0, 0)) V2I(xi % s, yi % s)
    else (V2I(xi % s, yi % s) + V2I(s, s)) % 2

  def inflate(height: Int): V3I =
    V3I(xi, height, yi)

  override protected def toIntsStrategy: V2I = this

  def until(o: V2I) =
    for {
      x <- xi until o.xi
      y <- yi until o.yi
    } yield V2I(x, y)

  def to(o: V2I) =
    for {
      x <- xi to o.xi
      y <- yi to o.yi
    } yield V2I(x, y)

  override def toString: String =
    "<" + xi + ", " + yi + ">"

}

object V2I {

  def apply(x: Int, y: Int) =
    new V2I(x, y)

  def unapply(v: V2I): Option[(Int, Int)] =
    Some((v.xi, v.yi))

}

object Origin2D extends V2I(0, 0)
object North2D extends V2I(0, 1)
object South2D extends V2I(0, -1)
object West2D extends V2I(-1, 0)
object East2D extends V2I(1, 0)