package com.phoenixkahlo.hellcraft.core.client

import java.util.concurrent.ThreadLocalRandom

import com.badlogic.gdx.graphics.Color
import com.phoenixkahlo.hellcraft.core.{Blocks, ChunkEvent, RenderWorld, World}
import com.phoenixkahlo.hellcraft.core.entity.{Avatar, EntID}
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._
import com.phoenixkahlo.hellcraft.core.client.ClientSessionData.{LoadDist, Sensitivity}

sealed trait ShouldCapture

case class AvatarClientMain(core: ClientCore, avaID: EntID[Avatar]) extends ClientLogic with ShouldCapture {
  val rands = ThreadLocal.withInitial[MRNG](() => new MRNG(ThreadLocalRandom.current.nextLong()))
  implicit def rand: MRNG = rands.get

  override def become(replacement: ClientLogic) =
    if (replacement.isInstanceOf[ShouldCapture])
      replacement -> Seq.empty
    else
      replacement -> Seq(CaptureCursor)

  override def frame(world: RenderWorld, input: ClientLogic.Input) = {
    val render: ClientLogic.RenderOutput = {
      val (wrender, globals) = core.render(world, input)
      val hud = core.hud("", Color.CLEAR, input)
      (wrender ++ hud, globals)
    }

    world.findEntity(avaID) match {
      case Some(ava) =>
        val pos = ava.pos//(world.interp)
        val result: ClientLogic.Output = {
          val forward = core.camDir.copy(y = 0).normalize
          var movDir: V3F = Origin
          if (core pressed W) movDir += forward
          if (core pressed S) movDir -= forward
          if (core pressed D) movDir += (forward cross Up).normalize
          if (core pressed A) movDir -= (forward cross Up).normalize

          val chunkPos = ava.chunkPos
          val target = (chunkPos - input.sessionData(LoadDist)) toAsSet (chunkPos + input.sessionData(LoadDist))

          (
            copy(core = core.copy(camPos = pos).update(world, input)),
            Seq(
              SetLoadTarget(target, target.bloat),
              CauseUpdateEffect(Avatar.setStride(ava, movDir * 5))
            )
          )
        }
        (result, render)
      case None =>
        println("cannot find avatar")
        (nothing, render)
    }
  }

  override def keyDown(keycode: KeyCode)(world: World, input: ClientLogic.Input) = keycode match {
    case ESCAPE => cause(ReleaseCursor)
    case SPACE => world.findEntity(avaID) match {
      case Some(ava) =>
        println("jumping")
        cause(CauseUpdateEffect(
          ChunkEvent.entEvent(ava.chunkPos, avaID)((ava, world) => ava.copy(vel = ava.vel.copy(y = 10)))
        ))
      case None =>
        println("jump failed, ava not found")
        nothing
    }
    case _ => become(copy(core = core.copy(pressed = core.pressed + keycode)))
  }

  override def keyUp(keycode: KeyCode)(world: World, input: ClientLogic.Input) =
    become(copy(core = core.copy(pressed = core.pressed - keycode)))

  override def touchDown(pos: V2I, pointer: KeyCode, button: Button)(world: World, input: ClientLogic.Input) = {
    if (input.isCursorCaught) button match {
      case Right =>
        world
          .placeBlock(core.camPos, core.camDir, 64)
          .map(v => cause(CauseUpdateEffect(ChunkEvent.setMat(v, Blocks.Brick, meshBlocksFast = true))))
          .getOrElse(nothing)
      case _ => nothing
    } else cause(CaptureCursor)
  }

  override def mouseMoved(pos: V2I, delta: V2I)(world: World, input: ClientLogic.Input) =
    if (input.isCursorCaught) {
      var camDir = core.camDir

      val dx = -delta.x * input.sessionData(Sensitivity)
      camDir = camDir.rotate(Up, dx)

      var dy = -delta.y * input.sessionData(Sensitivity)
      val awd = core.camDir angleWith Down
      val awu = core.camDir angleWith Up
      if (awd + dy < 1)
        dy = -awd + GodClient.margin
      else if (awu - dy < 1)
        dy = awu - GodClient.margin
      camDir = camDir.rotate(camDir cross Up, dy)

      become(copy(core = core.copy(camDir = camDir)))
    } else nothing

}
