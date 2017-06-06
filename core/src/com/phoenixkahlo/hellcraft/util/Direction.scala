package com.phoenixkahlo.hellcraft.util

sealed abstract class Direction(x: Int, y: Int, z: Int) extends V3I(x, y, z) {
  override def neg: Direction
}

case object Up extends Direction(0, 1, 0) {
  override def neg = Down
}
case object Down extends Direction(0, -1, 0) {
  override def neg = Up
}
case object North extends Direction(0, 0, 1) {
  override def neg = South
}
case object South extends Direction(0, 0, -1) {
  override def neg = North
}
case object East extends Direction(1, 0, 0) {
  override def neg = West
}
case object West extends Direction(-1, 0, 0) {
  override def neg = East
}

object Directions {

  val directions: List[Direction] = List(Up, Down, North, South, East, West)

  def apply(): List[Direction] = directions

}