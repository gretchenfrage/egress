package com.phoenixkahlo.hellcraft.core.entity

import com.phoenixkahlo.hellcraft.util.collections.{Identity, TypeMatchingMap}

case class EntityMap(raw: Map[AnyEntID, AnyEnt]) {
  def +(ent: AnyEnt): EntityMap =
    EntityMap(raw.updated(ent.id, ent))
    //EntityMap(raw + ((ent.id, ent)))

  def ++(ents: Seq[AnyEnt]): EntityMap =
    ???
    //EntityMap(raw ++ ents.map(ent => (ent.id, ent)))

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

/*
case class EntityMap(raw: Map[EntityID[Any], Entity[_ <: Entity[_]]]) {
  def +(ent: Entity[_ <: Entity[_]]): EntityMap =
    EntityMap(raw + ((ent.id, ent)))

  def ++(ents: Seq[Entity[_ <: Entity[_]]]): EntityMap =
    EntityMap(raw ++ ents.map(ent => (ent.id, ent)))

  def -(id: EntityID[_ <: Entity[_]]): EntityMap =
    EntityMap(raw - id)

  def --(ids: Seq[EntityID[_ <: Entity[_]]]): EntityMap =
    EntityMap(raw -- ids)

  def get[E <: Entity[E]](id: EntityID[E]): Option[E] =
    raw.get(id).map(_.asInstanceOf[E])

  def apply[E <: Entity[E]](id: EntityID[E]): E =
    raw(id).asInstanceOf[E]

  def keySet: Set[EntityID[_ <: Entity[_]]] =
    raw.keySet

  def values: Seq[Entity[_ <: Entity[_]]] =
    raw.values.toSeq

  def size: Int = raw.size

  def isEmpty: Boolean = raw.isEmpty

  def nonEmpty: Boolean = raw.nonEmpty
}

object EntityMap {
   val empty = EntityMap(Map.empty)
}

*/