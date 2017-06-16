package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.{Gdx, InputAdapter, InputProcessor}
import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.math.Vector3
import com.phoenixkahlo.hellcraft.util._

import scala.collection.mutable

case class AvatarController(
                        cam: Camera,
                        avatarID: UUID,
                        turnVel: Float = 0.25f,
                        offset: V3F = V3F(0, 1.75f, 0)
                      ) extends InputAdapter {

  private val pressed = new mutable.HashSet[Int]()

  override def keyDown(keycode: Int): Boolean =
    if (keycode == Keys.ESCAPE) {
      Gdx.input.setCursorCatched(false)
      true
    } else if (List(Keys.W, Keys.A, Keys.S, Keys.D, Keys.SHIFT_LEFT, Keys.SPACE, Keys.CONTROL_LEFT) contains keycode) {
      pressed.add(keycode)
      true
    } else false

  override def keyUp(keycode: Int): Boolean =
    pressed.remove(keycode)

  override def mouseMoved(screenX: Int, screenY: Int): Boolean = {
    if (Gdx.input.isCursorCatched) {
      val dx = -Gdx.input.getDeltaX * turnVel
      cam.direction.rotate(Up.toGdx, dx)

      // this bologne prevents you from looking straight up or down
      // which would make the camera's direction vector parallel with the camera's up vector
      def angle(v1: Vector3, v2: Vector3) = Math.toDegrees(Math.acos(v1.cpy().dot(v2) / (v1.len() * v2.len()))).toFloat
      val margin = 1
      var dy = -Gdx.input.getDeltaY * turnVel
      val angleWithDown = angle(Down toGdx, cam.direction)
      lazy val angleWithUp = angle(Up toGdx, cam.direction)
      if (angleWithDown + dy < margin)
        dy = -angleWithDown + margin
      else if (angleWithUp - dy < margin)
        dy = angleWithUp - margin
      cam.direction.rotate(cam.direction.cpy().crs(Up toGdx).nor(), dy)

      cam.update(true)
      true
    } else false
  }

  override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
    if (Gdx.input.isCursorCatched)
      false
    else {
      Gdx.input.setCursorCatched(true)
      true
    }

  def update(world: World): Seq[ChunkEvent] = {
    val camDirection = V3F(cam.direction).normalize

    var moveDirection: V3F = Origin
    if (pressed(Keys.W))
      moveDirection = moveDirection + camDirection
    if (pressed(Keys.S))
      moveDirection = moveDirection - camDirection
    if (pressed(Keys.D))
      moveDirection = moveDirection + (camDirection cross Up).normalize
    if (pressed(Keys.A))
      moveDirection = moveDirection - (camDirection cross Up).normalize

    val jumping = pressed(Keys.SPACE)

    val avatar = world.findEntity(avatarID).asInstanceOf[Avatar]
    Seq(ChunkEvent(avatar.chunkPos, _.putEntity(avatar.copy(
      "direction" -> moveDirection,
      "jumping" -> jumping
    ))))
  }

  def postUpdate(world: World): Unit = {
    val avatar = world.findEntity(avatarID).asInstanceOf[Avatar]
    cam.position.set((avatar.pos + offset).toGdx)
    cam.update()
  }

}
