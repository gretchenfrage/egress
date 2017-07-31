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
import com.phoenixkahlo.hellcraft.util.AsyncExecutor

import scala.collection.immutable.TreeMap

class ClientController(session: ServerSession, cam: Camera, val client: EgressClient) extends InputAdapter {

  val sensitivity = 0.25f
  val offset = V3F(0, 1.75f, 0)
  val margin = 1

  val avatarID = session.avatarID

  private val pressed = new mutable.HashSet[Int]
  private val clicks = new mutable.ArrayStack[Int]

  val keys = List(W, A, S, D, SHIFT_LEFT, SPACE, CONTROL_LEFT, TAB, H)

  //private var lastSetMovement: Long = Long.MinValue
  private val toSubmit = new LinkedBlockingQueue[(Long, ChunkEvent)]

  @volatile var camDir = V3F(cam.direction)

  new Thread(() => {
    while (true) {
      var events = new TreeMap[Long, Set[ChunkEvent]]
      def add(submission: (Long, ChunkEvent)): Unit = submission match {
        case (time, event) => events = events.updated(time, events.getOrElse(time, Set.empty) + event)
      }
      add(toSubmit.take())
      while (toSubmit.size > 0)
        add(toSubmit.remove())
      for (failed <- session.submitExterns(events))
        System.err.println("server rejected " + failed)
    }
  }, "controller event submission thread").start()

  override def keyDown(k: Int): Boolean =
    if (k == ESCAPE) {
      Gdx.input.setCursorCatched(false)
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
    world.findEntity(avatarID).map(_.asInstanceOf[Avatar]) match {
      case Some(avatar) =>
        var accumulator: SortedSet[ChunkEvent] = SortedSet.empty

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

        val jumping = pressed(SPACE)

        val setMovement = SetAvatarMovement(avatar.id, movDir, jumping, avatar.chunkPos, UUID.randomUUID())
        toSubmit.add((world.time, setMovement))
        accumulator += setMovement

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

        accumulator
      case None => SortedSet.empty
    }
  }

  def camUpdate(world: World, interpolation: Option[(World, Float)]): Unit = {
    world.findEntity(avatarID).map(_.asInstanceOf[Avatar]) match {
      case Some(avatar) =>
        this.camDir = V3F(cam.direction)

        val pos = interpolation.map({ case (a, b) => avatar.interpolatePos(a, b) }).getOrElse(avatar.pos)

        cam.position.set((pos + offset) toGdx)
        cam.update()
      case None =>
    }
  }

}
