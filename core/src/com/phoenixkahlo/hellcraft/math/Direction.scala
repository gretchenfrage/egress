package com.phoenixkahlo.hellcraft.math

/**
  * Unit vectors in the 6 directions
  */
sealed abstract class Direction(override val xi: Int, override val yi: Int, override val zi: Int) extends V3I(xi, yi, zi) {
  override def neg: Direction = this match {
    case Up => Down
    case Down => Up
    case North => South
    case South => North
    case East => West
    case West => East
  }

  override def toString: String = this match {
    case Up => "up"
    case Down => "down"
    case North => "north"
    case South => "south"
    case East => "east"
    case West => "west"
  }
}

object Up extends Direction(0, 1, 0)
object Down extends Direction(0, -1, 0)
object North extends Direction(0, 0, 1)
object South extends Direction(0, 0, -1)
object East extends Direction(-1, 0, 0)
object West extends Direction(1, 0, 0)

object Directions {
  val directions: List[Direction] = List(Up, Down, North, South, East, West)

  def apply(): List[Direction] = directions
}