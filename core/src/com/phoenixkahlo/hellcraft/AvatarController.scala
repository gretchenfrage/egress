package com.phoenixkahlo.hellcraft

import java.util.UUID

import com.badlogic.gdx.Input.{Buttons, Keys}
import com.badlogic.gdx.{Gdx, InputAdapter, InputProcessor}
import com.badlogic.gdx.graphics.{Camera, Color}
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
  private val clicks = new mutable.ArrayStack[Int]()


  override def keyDown(keycode: Int): Boolean =
    if (keycode == Keys.ESCAPE) {
      Gdx.input.setCursorCatched(false)
      true
    } else if (keycode == Keys.C) {
      val dir = V3F(cam.direction)
      println(Directions().map(d => (dir angleWith d, d)).sortBy(_._1).head._2)
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
    if (Gdx.input.isCursorCatched) {
      clicks.push(button)
      true
    } else {
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

    val avatarEvent = ChunkEvent(avatar.chunkPos, _.putEntity(avatar.copy(
      "direction" -> moveDirection,
      "jumping" -> jumping
    )))

    var events = Seq[ChunkEvent](avatarEvent)
    if (clicks.nonEmpty)
      clicks.pop() match {
        case Buttons.LEFT => Raytrace.hit(avatar.pos + offset, camDirection, world) match {
          case Some(v) => events = events ++ World.putBlock(v, Air)
          case None =>
        }
        case Buttons.RIGHT => Raytrace.place(avatar.pos + offset, camDirection, world) match {
          case Some(v) =>
            if (!(avatar.pos.y <= v.y + 1 && v.y <= avatar.pos.y + avatar.height &&
              RectangleProxmimity(Rectangle(v.flatten, (v + Ones).flatten), avatar.rad).contains(avatar.pos.flatten)))
              events = events ++ World.putBlock(v, Stone)
          case None =>
        }
      }

    events

    /*
    val breakEvent =
      if (Gdx.input.justTouched()) Raytrace.hit(avatar.pos + offset, camDirection, world).map(World.putBlock(_, Air))
      else None


    //Seq(avatarEvent, breakEvent)
    if (breakEvent isDefined) avatarEvent +: breakEvent.get
    else Seq(avatarEvent)
    */

    /*
    val intersecting = Raytrace(avatar.pos + offset, camDirection).takeWhile(world.blockAt(_).isDefined)
    event1 +: intersecting
      .map(BlockOutline(_, Color.RED))
      .map(outline => ChunkEvent(outline.chunkPos, _.putEntity(outline)))
      */
    /*

    val lookingAt = Raytrace.hit(avatar.pos + offset, camDirection, world)
    val outline = lookingAt.map(BlockOutline(_, Color.RED))
    val event2 = outline.map(e => ChunkEvent(e.chunkPos, _.putEntity(e)))

    if (event2.isDefined) Seq(event1, event2.get)
    else Seq(event1)
    */
  }

  def postUpdate(world: World): Unit = {
    val avatar = world.findEntity(avatarID).asInstanceOf[Avatar]
    cam.position.set((avatar.pos + offset).toGdx)
    cam.update()
  }

}
