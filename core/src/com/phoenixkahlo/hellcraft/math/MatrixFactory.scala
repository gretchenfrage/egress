package com.phoenixkahlo.hellcraft.math

import com.badlogic.gdx.math.Matrix4

object MatrixFactory {
  sealed trait Operation
  case class Rotate(axis: V3F, amount: Float) extends Operation
  case class Translate(by: V3F) extends Operation
  case class Scale(by: V3F) extends Operation

  def apply(operations: Operation*): Matrix4 =
    operations.foldRight(new Matrix4())((op, mat) => op match {
      case Rotate(axis, amount) => mat.rotate(axis.toGdx, amount)
      case Translate(by) => mat.translate(by.toGdx)
      case Scale(by) => mat.scale(by.x, by.y, by.z)
    })

}
