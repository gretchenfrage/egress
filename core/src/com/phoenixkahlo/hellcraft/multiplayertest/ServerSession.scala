package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID

import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.Avatar
import com.phoenixkahlo.hellcraft.math.{V3F, V3I}

trait ServerSession {

  def getTime: Long

  def chunkAt(time: Long, p: V3I): Option[Chunk]

  def getStarter(): (Long, Seq[Chunk])

  def avatarID: EntityID

  def setMovement(atTime: Long, movDir: V3F, jumping: Boolean): Boolean

  def submitExtern(event: ChunkEvent, atTime: Long): Boolean

  def avatarCount: Int

  def hashChunk(atTime: Long, p: V3I): Option[Int]

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
      server.continuum.waitForSnapshot(atTime).flatMap(_.chunkAt(p).map(_.hashCode()))
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

  override def submitExtern(event: ChunkEvent, atTime: Long): Boolean = {
    try {
      Thread.sleep(randLag)
      val accept = event match {
        case SetAvatarMovement(aID, _, _, _, _) if aID == avatarID => true
        case _ => false
      }
      if (accept)
        server.integrateExtern(atTime, event)
      else
        System.err.println("server: rejecting " + event)
      accept
    } finally Thread.sleep(randLag)
  }
}

