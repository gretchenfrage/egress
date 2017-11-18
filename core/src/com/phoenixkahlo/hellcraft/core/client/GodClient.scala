package com.phoenixkahlo.hellcraft.core.client
import com.phoenixkahlo.hellcraft.core.World
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.Input
import com.phoenixkahlo.hellcraft.math._
import com.badlogic.gdx.Input.Keys._

case class GodClient(pressed: Set[Int]) extends ClientLogic {
  override def update(world: World, input: Input) = {
    var movDir: V3F = Origin
    if (pressed(W)) movDir += input.camDir
    if (pressed(S)) movDir -= input.camDir
    if (pressed(D)) movDir += (input.camDir cross Up).normalize
    if (pressed(A)) movDir -= (input.camDir cross Up).normalize

    val chunkPos = input.camPos / 16 toInts
    val loadTarget = (chunkPos - GodClient.loadRad) to (chunkPos + GodClient.loadRad) toSet

    cause(
      SetCamPos(input.camPos + (movDir * GodClient.moveSpeed)),
      SetLoadTarget(loadTarget)
    )
  }

  override def keyDown(keycode: Int)(world: World, input: Input) =
    if (keycode == ESCAPE) cause(Exit)
    else become(copy(pressed = pressed + keycode))

  override def keyUp(keycode: Int)(world: World, input: Input) =
    become(copy(pressed = pressed - keycode))

  override def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input) =
    if (input.isCursorCaught) nothing
    else cause(CaptureCursor)

  override def touchDragged(pos: V2I, delta: V2I, pointer: Int)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    mouseMoved(pos, delta)(world, input)

  override def mouseMoved(pos: V2I, delta: V2I)(world: World, input: Input): (ClientLogic, Seq[ClientEffect]) =
    if (input.isCursorCaught) {
      var camDir = input.camDir

      val dx = -delta.x * GodClient.turnSpeed
      camDir = camDir.rotate(Up, dx)

      var dy = -delta.y * GodClient.turnSpeed
      val awd = input.camDir angleWith Down
      val awu = input.camDir angleWith Up
      if (awd + dy < GodClient.margin)
        dy = -awd + GodClient.margin
      else if (awu - dy < GodClient.margin)
        dy = awu - GodClient.margin
      camDir = camDir.rotate(camDir cross Up, dy)

      cause(SetCamDir(camDir))
    } else nothing


  //override def hud(world: World, input: Input) = super.hud(world)
}

object GodClient {
  val turnSpeed = 0.25f
  val moveSpeed = 1f
  val margin = 1f
  val loadRad = V3I(8, 4, 8)
}