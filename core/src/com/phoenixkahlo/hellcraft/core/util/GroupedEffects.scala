package com.phoenixkahlo.hellcraft.core.util

import com.phoenixkahlo.hellcraft.core.{UpdateEffect, UpdateEffectType}

import scala.collection.mutable

class GroupedEffects(unsorted: Seq[UpdateEffect]) {
  import UpdateEffectType._

  private val arr = new Array[mutable.Buffer[UpdateEffect]](UpdateEffectType.types.size);
  {
    for (i <- UpdateEffectType.types.indices)
      arr(i) = new mutable.ArrayBuffer
    for (effect <- unsorted)
      arr(effect.effectType.ord) += effect
  }

  def bin[T <: UpdateEffect](implicit effectType: UpdateEffectType[T]): Seq[T] =
    arr(effectType.ord).asInstanceOf[mutable.Buffer[T]]
}
