package com.phoenixkahlo.hellcraft.multiplayertest

import java.util.UUID
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.Camera
import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.math.Vector3
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, Entity}
import com.phoenixkahlo.hellcraft.core.{ChunkEvent, SetAvatarMovement, World}
import com.phoenixkahlo.hellcraft.util.AsyncExecutor

class ClientController(session: ServerSession, cam: Camera, val client: GameClient) extends InputAdapter {

  val sensitivity = 0.25f
  val offset = V3F(0, 1.75f, 0)
  val margin = 1

  val avatarID = session.avatarID

  private val pressed = new mutable.HashSet[Int]
  private val clicks = new mutable.ArrayStack[Int]

  val keys = List(W, A, S, D, SHIFT_LEFT, SPACE, CONTROL_LEFT, TAB, C, H)

  private val sendingSetMovement = new AtomicBoolean(false)

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

  def update(world: World, posHint: Option[V3I] = None): Unit = {
    (posHint.flatMap(world.chunkAt(_).get.entities.get(avatarID)) match {
      case option if option isDefined => option
      case None => world.findEntity(avatarID)
    }).map(_.asInstanceOf[Avatar]) match {
      case Some(avatar) =>
        val camDir = V3F(cam.direction).normalize

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

        /*
        if (!sendingSetMovement.getAndSet(true)) {
          AsyncExecutor run {
            if (!session.setMovement(world.time, movDir, jumping))
              println("server failed to set movement")
            sendingSetMovement.set(false)
          }
        }
        */
        val setMovement = SetAvatarMovement(avatar.id, movDir, jumping, avatar.chunkPos, UUID.randomUUID())
        if (!sendingSetMovement.getAndSet(true)) {
          AsyncExecutor run {
            val accepted = session.submitExtern(setMovement, world.time)
            if (!accepted)
              System.err.println("server rejected setmovement!")
            sendingSetMovement.set(false)
          }
          //client.getContinuum.integrate(setMovement, world.time)
        }

        val interpolation: Option[(World, Float)] =
          client.getContinuum.snapshot(world.time - 1) match {
            case Some(previous) => Some((previous, 1 - client.getClock.fractionalTicksSince(previous.time)))
            case None => None
          }

        val pos = interpolation.map({ case (a, b) => avatar.interpolatePos(a, b) }).getOrElse(avatar.pos)

        cam.position.set((pos + offset) toGdx)
        cam.update()

        // press c to count occurences of avatar on client and server, for debugging purposes
        if (pressed(C)) {
          pressed -= C

          println("client count: " + world.asInstanceOf[ClientWorld].getLoadedChunks.values.flatMap(_.entities.get(avatarID)).size)
          println("server count: " + session.avatarCount)
        }
        // press h to check for hashcode discrepencies between chunks in the client and server, for debugging purposes
        if (pressed(H)) {
          pressed -= H

          println("hash verifying")
          for (chunk <- world.asInstanceOf[ClientWorld].getLoadedChunks.values) {
            (chunk.hashCode(), session.hashChunk(world.time, chunk.pos)) match {
              case (n1, Some(n2)) if n1 != n2 => println("desynchronization at " + chunk.pos)
              case (_, None) => println("server failed to hash " + chunk.pos)
              case _ =>
            }
          }
          println("finished hash veryifying")
        }
      case None => println("controller failed to find avatar")
    }
  }

}
