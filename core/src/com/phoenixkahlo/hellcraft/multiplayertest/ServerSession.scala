package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

import scala.collection.immutable.SortedMap

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Boolean

  def submitExtern(event: ChunkEvent, atTime: Long): Boolean

  def submitExterns(events: SortedMap[Long, Set[ChunkEvent]]): Seq[(Long, ChunkEvent)]

  def avatarCount: Int

  def hashChunk(atTime: Long, p: V3I): Option[Int]

  def hashChunks(atTime: Long, chunks: Seq[V3I]): Map[V3I, Int]

  def getServerNanotime: Long

  def getGameclockStartNanotime: Long

}

class ServerSessionImpl(init: InitialClientData, server: GameServer, clientID: ClientID) extends ServerSession {

  override def getTime: Long = {
    try {
      Thread.sleep(randLag)
      server.clock.gametime
    } finally Thread.sleep(randLag)
  }

  override def chunkAt(time: Long, p: V3I): Option[Chunk] = {
    try {
      Thread.sleep(randLag)
      server.continuum.waitForSnapshot(time).flatMap(_.chunkAt(p))
    } finally Thread.sleep(randLag)
  }

  override def getStarter(): (Long, Seq[Chunk]) = {
    try {
      Thread.sleep(randLag)
      server.continuum.getStarter(clientID)
    } finally Thread.sleep(randLag)
  }

  lazy val avatarID: EntityID = {
    try {
      Thread.sleep(randLag)
      server.avatars.synchronized {
        while (!server.avatars.contains(clientID))
          server.avatars.wait()
        server.avatars(clientID)
      }
    } finally Thread.sleep(randLag)
  }

  override def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Boolean = {
    try {
      Thread.sleep(randLag)
      server.continuum.waitForSnapshot(atTime).flatMap(_.findEntity(avatarID).map(_.asInstanceOf[Avatar])) match {
        case Some(avatar) =>
          server.integrateExtern(atTime,
            SetAvatarMovement(avatar.id, movDir, jumping, avatar.chunkPos, UUID.randomUUID()))
          true
        case None => false
      }
    } finally Thread.sleep(randLag)
  }

  override def avatarCount: Int = {
    try {
      Thread.sleep(randLag)
      server.continuum.current.loaded.values.flatMap(_.entities.get(avatarID)).size
    } finally Thread.sleep(randLag)
  }

  override def hashChunk(atTime: Long, p: V3I): Option[Int] = {
    try {
      Thread.sleep(randLag)
      server.continuum.snapshot(atTime).flatMap(_.chunkAt(p).map(_.hashCode()))
    } finally Thread.sleep(randLag)
  }

  override def hashChunks(atTime: Long, chunks: Seq[V3I]): Map[V3I, Int] = {
    try {
      Thread.sleep(randLag)
      server.continuum.snapshot(atTime) match {
        case Some(world) => chunks.flatMap(world.chunkAt(_).map(chunk => (chunk.pos, chunk.hashCode))).toMap
        case None => Map.empty
      }
    } finally Thread.sleep(randLag)
  }

  override def getServerNanotime: Long = {
    try {
      Thread.sleep(randLag)
      System.nanoTime()
    } finally Thread.sleep(randLag)
  }

  override def getGameclockStartNanotime: Long = {
    try {
      Thread.sleep(randLag)
      server.clock.nanotimeStart
    } finally Thread.sleep(randLag)
  }

  private def isLegit(extern: ChunkEvent): Boolean = extern match {
    case SetAvatarMovement(aID, _, _, _, _) if aID == avatarID => true
    case _ => false
  }


  override def submitExtern(event: ChunkEvent, atTime: Long): Boolean = {
    try {
      Thread.sleep(randLag)
      val accept = isLegit(event)
      if (accept) {
        server.integrateExtern(atTime, event)
      }
      accept
    } finally Thread.sleep(randLag)
  }


  override def submitExterns(events: SortedMap[Long, Set[ChunkEvent]]): Seq[(Long, ChunkEvent)] = {
    try {
      Thread.sleep(randLag)
      // filter which ones to accept
      val accepted = events.mapValues(_.filter(isLegit)).filterNot({ case (_, set) => set isEmpty })
      // integrate then
      if (accepted nonEmpty)
        server.integrateExterns(accepted)
      // compute which ones were rejected and send back
      events.toSeq.flatMap({ case (time, set) => set.map((time, _)) })
        .filterNot({ case (time, event) => accepted.getOrElse(time, Set.empty).contains(event) })
    } finally Thread.sleep(randLag)
  }


}

