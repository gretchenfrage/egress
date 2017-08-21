package com.phoenixkahlo.hellcraft.util.caches

import scala.collection.mutable

class KeyParamPool[K,P,T](factory: P => T) {

  private val pool = new mutable.HashMap[K,T]()

  def apply(key: K, params: P): T =
    this.synchronized {
      if (pool contains key) pool(key)
      else {
        val t = factory(params)
        pool.put(key, t)
        t
      }
    }

}
