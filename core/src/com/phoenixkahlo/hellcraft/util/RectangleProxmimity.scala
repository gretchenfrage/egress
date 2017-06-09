package com.phoenixkahlo.hellcraft.util

/**
  * The area within a certain distance of a rectangle
  */
case class RectangleProxmimity(
                              rect: Rectangle,
                              r: Float
                              ) {

  private val verticalRect = Rectangle(rect.min - V2F(0, r), rect.max + V2F(0, r))
  private val horizRect = Rectangle(rect.min - V2F(r, 0), rect.max + V2F(r, 0))
  private val quad3Circle = Circle(rect.min, r)
  private val quad1Circle = Circle(rect.max, r)
  private val quad2Circle = Circle(V2F(rect.min.x, rect.max.y), r)
  private val quad4Circle = Circle(V2F(rect.max.x, rect.min.y), r)

  def contains(p: V2F): Boolean =
    ((verticalRect contains p)
      || (horizRect contains p)
      || (quad3Circle contains p)
      || (quad1Circle contains p)
      || (quad2Circle contains p)
      || (quad4Circle contains p))

  def closestPerimiterPoint(p: V2F): V2F = {
    /*
    lazy val angle = (p - rect.center).direction
    lazy val pointingRight = angle < 45 || angle >= 315
    lazy val pointingUp = angle >= 45 && angle < 135
    lazy val pointingLeft = angle >= 135 && angle < 225
    lazy val pointingDown = angle >= 225 && angle < 315

    if (p > quad1Circle.center)
      (p - quad1Circle.center).normalize * r + quad1Circle.center
    else if (p < quad3Circle.center)
      (p - quad3Circle.center).normalize * r + quad3Circle.center
    else if (p <> quad2Circle.center)
      (p - quad2Circle.center).normalize * r + quad2Circle.center
    else if (p >< quad4Circle.center)
      (p - quad4Circle.center).normalize * r + quad4Circle.center
    else if (pointingUp)
      V2F(p.x, rect.max.y + r)
    else if (pointingDown)
      V2F(p.x, rect.min.y - r)
    else if (pointingLeft)
      V2F(rect.min.x - r, p.y)
    else if (pointingRight)
      V2F(rect.max.x + r, p.y)
    else {
      println("using fallback")
      V2F(rect.max.x + r, p.y)
    }
    */
    if (p > quad1Circle.center)
      (p - quad1Circle.center).normalize * r + quad1Circle.center
    else if (p < quad3Circle.center)
      (p - quad3Circle.center).normalize * r + quad3Circle.center
    else if (p <> quad2Circle.center)
      (p - quad2Circle.center).normalize * r + quad2Circle.center
    else if (p >< quad4Circle.center)
      (p - quad4Circle.center).normalize * r + quad4Circle.center
    else
      p.closest(
        V2F(p.x, rect.max.y + r),
        V2F(p.x, rect.min.y - r),
        V2F(rect.min.x - r, p.y),
        V2F(rect.max.x + r, p.y)
      )
  }

}
