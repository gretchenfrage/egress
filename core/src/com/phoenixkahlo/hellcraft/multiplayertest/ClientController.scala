package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.Camera
import com.phoenixkahlo.hellcraft.math._

import scala.collection.{SortedSet, mutable}
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.math.Vector3
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, Entity}
import com.phoenixkahlo.hellcraft.core.{Chunk, ChunkEvent, SetAvatarMovement, World}
import com.phoenixkahlo.hellcraft.menu.MainMenu
import com.phoenixkahlo.hellcraft.util.{AsyncExecutor, Cache}

import scala.collection.immutable.TreeMap

class ClientController(cam: Camera, client: EgressClient) extends InputAdapter {

  val session = client.session
  val sessionNoReply = client.sessionNoReply

  val sensitivity = 0.25f
  val offset = V3F(0, 1.75f, 0)
  val margin = 1

  val avatarID = session.avatarID

  private val pressed = new mutable.HashSet[Int]
  private val clicks = new mutable.ArrayStack[Int]

  val keys = List(W, A, S, D, SHIFT_LEFT, SPACE, CONTROL_LEFT, TAB, H, C, F1)



  @volatile var camDir = V3F(cam.direction)
  @volatile var movDir: V3F = Origin

  @volatile var thirdPerson = false

  def submit(atTime: Long, event: ChunkEvent): Unit = {
    //sessionNoReply.submitExtern(event, atTime)
    //udpSession.submitExtern(event, atTime)
    sessionNoReply.submitExtern(event, atTime)
    //sessionNoReply.printReceiveDelta(System.nanoTime())
    //sessionNoReply.printReceiveDelta(client.serverNanotime.nanotime)
  }

  override def keyDown(k: Int): Boolean =
    if (k == F1) {
      Gdx.input.setCursorCatched(false)
      true
    } else if (k == ESCAPE) {
      client.getDriver.enter(new MainMenu(new Cache(client.getResources)))
      true
    } else if (k == F5) {
      thirdPerson = !thirdPerson
      true
    } else if (keys contains k) {
      pressed += k
      true
    } else false

  override def keyUp(k: Int): Boolean =
    pressed remove k

  override def mouseMoved(screenX: Int, screenY: Int): Boolean =
    if (Gdx.input.isCursorCatched) {
      val dx = -Gdx.input.getDeltaX * sensitivity
      cam.direction.rotate(Up toGdx, dx)

      var dy = -Gdx.input.getDeltaY * sensitivity
      val angleWithDown = V3F(cam.direction) angleWith Down
      lazy val angleWithUp = V3F(cam.direction) angleWith Up
      if (angleWithDown + dy < margin)
        dy = -angleWithDown + margin
      else if (angleWithUp - dy < margin)
        dy = angleWithUp - margin
      cam.direction.rotate(cam.direction.cpy().crs(Up toGdx).nor(), dy)

      cam.update(true)
      true
    } else false

  override def touchDragged(x: Int, y: Int, p: Int): Boolean = mouseMoved(x, y)

  override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
    if (Gdx.input.isCursorCatched) {
      clicks.push(button)
      true
    } else {
      Gdx.input.setCursorCatched(true)
      true
    }

  def mainUpdate(world: World): SortedSet[ChunkEvent] = {
    // compute the movement direction
    val camDir = this.camDir

    var movDir: V3F = Origin
    if (pressed(W))
      movDir += camDir
    if (pressed(S))
      movDir -= camDir
    if (pressed(D))
      movDir += (camDir cross Up).normalize
    if (pressed(A))
      movDir -= (camDir cross Up).normalize

    this.movDir = movDir
    // press h to check for hashcode discrepencies between chunks in the client and server, for debugging purposes
    if (pressed(H)) {
      pressed -= H

      println("hash verifying")
      val chunks = world.asInstanceOf[ClientWorld].getLoadedChunks.keys.toSeq
      val hashes = session.hashChunks(world.time, chunks)
      for (p <- chunks) {
        (world.chunkAt(p).get.hashCode, hashes.get(p)) match {
          case (n1, Some(n2)) if n1 != n2 => println("desynchronization at " + p)
          case (_, None) => println("server failed to hash " + p)
          case _ =>
        }
      }
      println("finished hash veryifying")
    }
    // press C to count avatar occurences on client and server
    if (pressed(C)) {
      pressed -= C

      println("client avatars in: " +
        world.asInstanceOf[ClientWorld].getLoadedChunks.values.filter(_.entities.contains(avatarID)).map(_.pos))
      println("server avatars in: " + session.findEntity(world.time, avatarID))
    }
    world.findEntity(avatarID).map(_.asInstanceOf[Avatar]) match {
      case Some(avatar) =>
        var accumulator: SortedSet[ChunkEvent] = SortedSet.empty

        val jumping = pressed(SPACE)

        val setMovement = SetAvatarMovement(avatar.id, movDir, jumping, avatar.chunkPos, UUID.randomUUID())
        submit(world.time, setMovement)
        accumulator += setMovement

        if (PredictionEnabled) accumulator
        else SortedSet.empty
      case None =>
        //println("can't find avatar (main update)")
        SortedSet.empty
    }
  }

  def camUpdate(world: World, interpolation: Option[(World, Float)]): Unit = {
    this.camDir = V3F(cam.direction)
    world.findEntity(avatarID).map(_.asInstanceOf[Avatar]) match {
      case Some(avatar) =>
        val pos = interpolation.map({ case (a, b) => avatar.interpolatePos(a, b) }).getOrElse(avatar.pos)
        if (thirdPerson)
          cam.position.set((pos - (camDir.normalize * 5)) toGdx)
        else
          cam.position.set((pos + offset) toGdx)
        cam.update()
      case None =>
        var pos = V3F(cam.position)
        pos += movDir * 0.1f
        cam.position.set(pos toGdx)
        cam.update()
    }
  }

}
