package com.phoenixkahlo.hellcraft.util

class ParamCache[P,T](factory: P => T) {

  private var value: Option[T] = None

  def apply(param: P) = {
    val p = Profiler("param cache apply")
    try {
      this.synchronized {
        p.log()
        if (value.isEmpty) value = Some(factory(param))
        p.log()
        value.get
      }
    } finally {
      p.log()
      p.printDisc(1)
    }
  }

  def invalidate =
    this.synchronized {
      value = None
    }

}
