package com.phoenixkahlo.hellcraft.util.fields

import com.phoenixkahlo.hellcraft.carbonite.CarboniteFields
import com.phoenixkahlo.hellcraft.math.V3I

trait IDMapping[T] extends Serializable {
  def id(item: T): Byte
  def lookup(id: Byte): T
}

/**
  * Given an enumeration of objects with 256-bit IDs, this object will represent a field of them, backed by a
  * ByteField.
  */
@CarboniteFields
case class IDField[T] private[fields](bytes: ByteField, mapping: IDMapping[T]) {

  def size: V3I = bytes.size

  def updated(v: V3I, item: T): IDField[T] =
    IDField[T](bytes.updated(v, mapping.id(item)), mapping)

  def get(v: V3I): Option[T] =
    bytes.get(v).map(mapping.lookup)

  def apply(v: V3I): T =
    mapping.lookup(bytes(v))

  def atMod(v: V3I): T =
    mapping.lookup(bytes.atMod(v))

}

object IDField {
  def apply[T](size: V3I, gen: V3I => T)(implicit mapping: IDMapping[T]): IDField[T] =
    IDField(ByteField(size, gen andThen mapping.id), mapping)

  def apply[T](size: V3I, const: T)(implicit mapping: IDMapping[T]): IDField[T] =
    IDField(ByteField(size, mapping.id(const)), mapping)
}