package com.phoenixkahlo.hellcraft.util.threading

/**
  * Seq[Fut[T]] => Fut[Seq[T]]
  */
object FutSeqFold {
  val empty = Fut(Seq.empty, _.run())

  def apply[T](in: Seq[Fut[T]], exec: Runnable => Unit): Fut[Seq[T]] =
    in.map(_.map(Seq(_), exec)).fold(empty)((f1, f2) => f1.flatMap(s1 => f2.map(s2 => s1 ++ s2, exec)))
}
