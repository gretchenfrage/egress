package com.phoenixkahlo.hellcraft.multiplayertest

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.Camera
import com.phoenixkahlo.hellcraft.math._

import scala.collection.mutable
import com.badlogic.gdx.Input.Keys._
import com.badlogic.gdx.math.Vector3
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, Entity}
import com.phoenixkahlo.hellcraft.core.{ChunkEvent, World}

class ClientController(session: ServerSession, cam: Camera, val client: GameClient) extends InputAdapter {

  val sensitivity = 0.25f
  val offset = V3F(0, 1.75f, 0)
  val margin = 1

  val avatarID = session.avatarID

  private val pressed = new mutable.HashSet[Int]
  private val clicks = new mutable.ArrayStack[Int]

  val keys = List(W, A, S, D, SHIFT_LEFT, SPACE, CONTROL_LEFT, TAB, C, H)

  private var lastMovDir: Option[V3F] = None
  private var lastJumping: Option[Boolean] = None

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

        if (!(lastMovDir.contains(movDir) && lastJumping.contains(jumping))) {
          session.setMovement(world.time, movDir, jumping)
          lastMovDir = Some(movDir)
          lastJumping = Some(jumping)
        }

        cam.position.set((avatar.pos + offset) toGdx)
        cam.update()

        if (pressed(C)) {
          pressed -= C

          println("client count: " + world.asInstanceOf[ClientWorld].getLoadedChunks.values.flatMap(_.entities.get(avatarID)).size)
          println("server count: " + session.avatarCount)
        }

        if (pressed(H)) {
          pressed -= H

          println("hash verifying")
          for (chunk <- world.asInstanceOf[ClientWorld].getLoadedChunks.values) {
            (chunk.hashCode(), session.hashChunk(world.time, chunk.pos)) match {
              case (n1, Some(n2)) if n1 != n2 => println("desynchronization at " + chunk.pos)
              case _ =>
            }
          }
          println("finished hash veryifying")
        }
      case None => println("controller failed to find avatar")
    }
  }

}
