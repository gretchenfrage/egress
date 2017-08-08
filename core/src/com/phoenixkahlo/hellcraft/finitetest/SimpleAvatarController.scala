package com.phoenixkahlo.hellcraft.finitetest

import java.util.UUID

import com.badlogic.gdx.Input.{Buttons, Keys}
import com.badlogic.gdx.graphics.{Camera, Color}
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, BlockOutline}
import com.phoenixkahlo.hellcraft.math._
import com.phoenixkahlo.hellcraft.util.RNG

import scala.collection.mutable

case class SimpleAvatarController(
                        cam: Camera,
                        avatarID: UUID,
                        exitor: Runnable,
                        turnVel: Float = 0.25f,
                        offset: V3F = V3F(0, 1.75f, 0)
                      ) extends InputAdapter {

  private val pressed = new mutable.HashSet[Int]()
  private val clicks = new mutable.ArrayStack[Int]()

  val keys = List(Keys.W, Keys.A, Keys.S, Keys.D, Keys.SHIFT_LEFT, Keys.SPACE, Keys.CONTROL_LEFT, Keys.TAB, Keys.F1)

  override def keyDown(keycode: Int): Boolean =
    if (keycode == Keys.F1) {
      Gdx.input.setCursorCatched(false)
      true
    } else if (keycode == Keys.ESCAPE) {
      exitor.run()
      true
    } else if (keycode == Keys.C) {
      val dir = V3F(cam.direction)
      println(Directions().map(d => (dir angleWith d, d)).minBy(_._1)._2)
      true
    } else if (keys contains keycode) {
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


  override def touchDragged(screenX: Int, screenY: Int, pointer: Int): Boolean =
    mouseMoved(screenX, screenY)

  override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
    if (Gdx.input.isCursorCatched) {
      clicks.push(button)
      true
    } else {
      Gdx.input.setCursorCatched(true)
      true
    }

  def update(world: World, lastAvatarChunkPos: Option[V3I] = None): Seq[ChunkEvent] = {
    val avatar = lastAvatarChunkPos.flatMap(world.chunkAt(_).flatMap(_.entities.get(avatarID)))
      .getOrElse(world.findEntity(avatarID).get).asInstanceOf[Avatar]

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

    //val avatarEvent = ChunkEvent(avatar.chunkPos, UUID.randomUUID(), _.putEntity(
    //  avatar.updateDirection(moveDirection).updateJumping(jumping)))
    val avatarEvent = AddEntity(avatar.updateDirection(moveDirection).updateJumping(jumping), UUID.randomUUID())

    var events = Seq[ChunkEvent](avatarEvent)
    val m = 1e-3f
    if (clicks.nonEmpty) {
      clicks.pop() match {
        case Buttons.LEFT => Raytrace.hit(avatar.pos + offset, camDirection, world) match {
          case Some(v) => events = events ++ World.putBlock(v, Air, RNG.uuids(RNG(System.nanoTime())))
          case None =>
        }
        case Buttons.RIGHT => Raytrace.place(avatar.pos + offset, camDirection, world) match {
          case Some(v) =>
            if (!(avatar.pos.y + m <= v.y + 1 - m && v.y + m <= avatar.pos.y + avatar.height - m &&
              RectangleProxmimity(Rectangle(v.flatten, (v + Ones).flatten), avatar.rad - m).contains(avatar.pos.flatten)))
              events = events ++ World.putBlock(v, Brick, RNG.uuids(RNG(System.nanoTime())))
          case None =>
        }
        case _ =>
      }
    }


    Raytrace.hit(avatar.pos + offset, camDirection, world) match {
      case Some(v) =>
        val outline = BlockOutline(v, Color.BLACK)
        events = events :+ AddEntity(outline, UUID.randomUUID())//ChunkEvent(outline.chunkPos, UUID.randomUUID(),  _.putEntity(outline))
      case None =>
    }
    if (pressed(Keys.TAB)) {
      Raytrace.place(avatar.pos + offset, camDirection, world) match {
        case Some(v) =>
          val outline = BlockOutline(v, Color.RED)
          events = events :+ AddEntity(outline, UUID.randomUUID())//ChunkEvent(outline.chunkPos, UUID.randomUUID(), _.putEntity(outline))
        case None =>
      }
    }


    events
  }

  def postUpdate(world: World, lastAvatarChunkPos: Option[V3I] = None): Unit = {
    //val avatar = avatarCache.getOrElse(world.findEntity(avatarID).asInstanceOf[Avatar])
    val avatar = lastAvatarChunkPos.flatMap(world.chunkAt(_).flatMap(_.entities.get(avatarID)))
      .getOrElse(world.findEntity(avatarID).get).asInstanceOf[Avatar]
    cam.position.set((avatar.pos + offset).toGdx)
    cam.update()
  }

  val hud = new DefaultHUD


}
