package com.phoenixkahlo.hellcraft.singleplayer

import java.util.UUID
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.badlogic.gdx.Input.{Buttons, Keys}
import com.badlogic.gdx.{Gdx, InputAdapter}
import com.badlogic.gdx.graphics.{Camera, Color, PerspectiveCamera}
import com.phoenixkahlo.hellcraft.core._
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, BlockOutline}
import com.phoenixkahlo.hellcraft.util.{RNG, SpatialExecutor}

import scala.collection.{SortedSet, mutable}

class SingleplayerController(cam: PerspectiveCamera, val avatarID: AvatarID, exitor: Runnable) extends InputAdapter {

  val TurnVel = 0.25f
  val Offset = V3F(0, 1.75f, 0)
  val Margin: Float = 1

  val ValidKeys = Set(W, A, S, D, SPACE, CONTROL_LEFT, SHIFT_LEFT, TAB, F1, F5)

  private val lock = new ReentrantReadWriteLock
  private val pressedSet = new mutable.HashSet[Int]
  private val clickStack = new mutable.ArrayStack[Int]

  @volatile var camDir = V3F(cam.direction)
  @volatile var movDir: V3F = Origin

  @volatile var thirdPerson = false

  def pressed(keycode: Int): Boolean = {
    try {
      lock.readLock.lock()
      pressedSet contains keycode
    } finally lock.readLock.unlock()
  }

  def popClick(): Option[Int] = {
    try {
      lock.readLock.lock()
      if (clickStack nonEmpty) Some(clickStack.pop())
      else None
    } finally lock.readLock.unlock()
  }

  override def keyDown(keycode: Int): Boolean =
    if (keycode == Keys.F1) {
      Gdx.input.setCursorCatched(false)
      true
    } else if (keycode == F5) {
      thirdPerson = !thirdPerson
      println("third person = " + thirdPerson)
      true
    } else if (keycode == Keys.ESCAPE) {
      exitor.run()
      true
    } else if (keycode == Keys.C) {
      val dir = V3F(cam.direction)
      println(Directions().map(d => (dir angleWith d, d)).minBy(_._1)._2)
      true
    } else if (ValidKeys contains keycode) {
      lock.writeLock.lock()
      pressedSet.add(keycode)
      lock.writeLock.unlock()
      true
    } else false

  override def keyUp(keycode: Int): Boolean = {
    try {
      lock.writeLock.lock()
      pressedSet.remove(keycode)
    } finally lock.writeLock().unlock()
  }

  override def mouseMoved(screenX: Int, screenY: Int): Boolean =
    if (Gdx.input.isCursorCatched) {
      val dx = -Gdx.input.getDeltaX * TurnVel
      cam.direction.rotate(Up toGdx, dx)

      var dy = -Gdx.input.getDeltaY * TurnVel
      val angleWithDown = V3F(cam.direction) angleWith Down
      lazy val angleWithUp = V3F(cam.direction) angleWith Up
      if (angleWithDown + dy < Margin)
        dy = -angleWithDown + Margin
      else if (angleWithUp - dy < Margin)
        dy = angleWithUp - Margin
      cam.direction.rotate(cam.direction.cpy().crs(Up toGdx).nor(), dy)

      cam.update(true)
      true
    } else false

  override def touchDragged(screenX: Int, screenY: Int, pointer: Int): Boolean =
    mouseMoved(screenX, screenY)

  override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int): Boolean =
    if (Gdx.input.isCursorCatched) {
      lock.writeLock.lock()
      clickStack.push(button)
      lock.writeLock.unlock()
      true
    } else {
      Gdx.input.setCursorCatched(true)
      true
    }

  def mainUpdate(world: LazyInfWorld): Seq[ChunkEvent] = {
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

    val avatar = world.findEntity(avatarID).get.asInstanceOf[Avatar]
    var accumulator: Seq[ChunkEvent] = Seq.empty

    val jumping = pressed(SPACE)
    val sprinting = pressed(SHIFT_LEFT)

    accumulator +:= SetAvatarMovement(avatarID, movDir, jumping, sprinting, UUID.randomUUID(), avatar.chunkPos)

    if (jumping && sprinting)
      accumulator +:= ThrustCylindroid(avatarID, V3F(0, 0.6f, 0), avatar.chunkPos, UUID.randomUUID())

    Raytrace.hit(avatar.pos + Offset, camDir, world).foreach(v => {
      accumulator +:= AddEntity(BlockOutline(v, Color.BLACK), UUID.randomUUID())
    })
    if (pressed(TAB)) Raytrace.place(avatar.pos + Offset, camDir, world).foreach(v => {
      accumulator +:= AddEntity(BlockOutline(v, Color.RED), UUID.randomUUID())
    })

    var click: Option[Int] = null
    do {
      click = popClick()
      click.foreach({
        case Buttons.LEFT => Raytrace.hit(avatar.pos + Offset, camDir, world).foreach(v => {
          accumulator ++= World.putBlock(v, Air, RNG.uuids(RNG(System.nanoTime())))
        })
        case Buttons.RIGHT => Raytrace.place(avatar.pos + Offset, camDir, world).foreach(v => {
          val m = 1e-3f
          if (!(avatar.pos.y + m <= v.y + 1 - m && v.y + m <= avatar.pos.y + avatar.height - m &&
            RectangleProxmimity(Rectangle(v.flatten, (v + Ones).flatten), avatar.rad - m).contains(avatar.pos.flatten)))
            accumulator ++= World.putBlock(v, Brick, RNG.uuids(RNG(System.nanoTime())))
        })
      })
    } while (click.isDefined)

    if (Gdx.input.isKeyJustPressed(Keys.P)) {
      println(SpatialExecutor.global)
    }

    accumulator
  }

  def camUpdate(world: LazyInfWorld, interpolation: Option[(World, Float)]): Unit = {
    this.camDir = V3F(cam.direction)
    val avatar = world.findEntity(avatarID).get.asInstanceOf[Avatar]

    val pos = interpolation.map({ case (a, b) => avatar.interpolatePos(a, b) }).getOrElse(avatar.pos)
    if (!thirdPerson) cam.position.set((pos + Offset) toGdx)
    else cam.position.set((pos + Offset - (camDir.normalize * 5)) toGdx)

    def keyFov(w: World): Float =
      if (world.findEntity(avatarID).get.asInstanceOf[Avatar].sprinting) 92
      else 90
    val fov: Float =
      interpolation.map({ case (w, f) => (keyFov(w) - keyFov(world)) * f + keyFov(world) }).getOrElse(keyFov(world))
    cam.fieldOfView = fov

    cam.update()
  }

}
