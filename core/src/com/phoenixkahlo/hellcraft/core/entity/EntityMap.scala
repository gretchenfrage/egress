package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.util.collections.{Identity, TypeMatchingMap}

case class EntityMap(raw: Map[AnyEntID, AnyEnt]) {
  def +(ent: AnyEnt): EntityMap =
    EntityMap(raw.updated(ent.id, ent))

  def ++(ents: Seq[AnyEnt]): EntityMap =
    ents.foldLeft(this)(_ + _)

  def -(id: AnyEntID): EntityMap =
    EntityMap(raw - id)

  def --(ids: Seq[AnyEntID]): EntityMap =
    EntityMap(raw -- ids)

  def get[E <: Entity[E]](id: EntID[E]): Option[E] =
    raw.get(id).map(_.asInstanceOf[E])

  def apply[E <: Entity[E]](id: EntID[E]): E =
    raw(id).asInstanceOf[E]

  def keySet: Set[AnyEntID] =
    raw.keySet

  def values: Seq[AnyEnt] =
    raw.values.toSeq

  def size: Int = raw.size

  def isEmpty: Boolean = raw.isEmpty

  def nonEmpty: Boolean = raw.nonEmpty
}

object EntityMap {
  val empty = EntityMap(Map.empty)
}