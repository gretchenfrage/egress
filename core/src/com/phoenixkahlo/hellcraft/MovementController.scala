package com.phoenixkahlo.hellcraft

import com.badlogic.gdx.Input.Keys
import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.math.{MathUtils, Vector3}
import com.badlogic.gdx.{Gdx, Input, InputAdapter, InputProcessor}
import com.phoenixkahlo.hellcraft.util.{Down, Up}

import scala.collection.mutable

class MovementController(
                        val cam: Camera,
                        val moveVel: Float = 10,
                        val sprintVel: Float = 20,
                        val turnVel: Float = 0.25f
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
      val angleWithDown = angle(Down.toGdx, cam.direction)
      lazy val angleWithUp = angle(Up.toGdx, cam.direction)
      if (angleWithDown + dy < margin)
        dy = -angleWithDown + margin
      else if (angleWithUp - dy < margin)
        dy = angleWithUp - margin
      cam.direction.rotate(cam.direction.cpy().crs(Up.toGdx).nor(), dy)

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

  def update(): Unit = {
    val vel = if (pressed(Keys.CONTROL_LEFT)) sprintVel else moveVel

    cam.up.set(Up.toGdx)
    def horizontalNormal(v: Vector3) =
      new Vector3(v.x, 0, v.z).nor()
    if (pressed(Keys.W))
      cam.position.add(horizontalNormal(cam.direction).scl(vel * Gdx.graphics.getDeltaTime))
    if (pressed(Keys.S))
      cam.position.add(horizontalNormal(cam.direction).scl(-vel * Gdx.graphics.getDeltaTime))
    if (pressed(Keys.A))
      cam.position.add(cam.direction.cpy().crs(Up.toGdx).nor().scl(-vel * Gdx.graphics.getDeltaTime))
    if (pressed(Keys.D))
      cam.position.add(cam.direction.cpy().crs(Up.toGdx).nor().scl(vel * Gdx.graphics.getDeltaTime))
    if (pressed(Keys.SPACE))
      cam.position.add(Up.toGdx.nor().scl(vel * Gdx.graphics.getDeltaTime))
    if (pressed(Keys.SHIFT_LEFT))
      cam.position.add(Up.toGdx.nor().scl(-vel * Gdx.graphics.getDeltaTime))
    cam.update(true)
  }

}
