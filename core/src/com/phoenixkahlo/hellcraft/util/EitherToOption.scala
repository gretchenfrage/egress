package com.phoenixkahlo.hellcraft.util

object LeftOption {
  def apply[T](either: Either[T, _]): Option[T] = either match {
    case Left(t) => Some(t)
    case Right(_) => None
  }
}

object RightOption {
  def apply[T](either: Either[_, T]): Option[T] = either match {
    case Left(_) => None
    case Right(t) => Some(t)
  }
}