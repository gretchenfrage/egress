package com.phoenixkahlo.hellcraft.util

object MergeBinned {

  def apply[K, V](map1: Map[K, Seq[V]], map2: Map[K, Seq[V]]): Map[K, Seq[V]] =
    (map1.keySet ++ map2.keySet).toSeq
      .map(key => key -> (map1.getOrElse(key, Seq.empty) ++ map2.getOrElse(key, Seq.empty))).toMap

}
