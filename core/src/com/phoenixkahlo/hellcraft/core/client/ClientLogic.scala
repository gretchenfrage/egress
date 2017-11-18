package com.phoenixkahlo.hellcraft.core.client

import com.badlogic.gdx.Input.Buttons
import com.phoenixkahlo.hellcraft.core.{UpdateEffect, World}
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.{Input, Output}
import com.phoenixkahlo.hellcraft.graphics.{EmptyHUD, HUD, RenderUnit}
import com.phoenixkahlo.hellcraft.math.{V2I, V3F, V3I}

sealed trait ClientEffect
case class CauseUpdateEffect(effects: Seq[UpdateEffect]) extends ClientEffect
object CauseUpdateEffect {
  def apply(effect: UpdateEffect): CauseUpdateEffect = CauseUpdateEffect(Seq(effect))
}
case class SetLoadTarget(target: Set[V3I]) extends ClientEffect
case class SetCamPos(p: V3F) extends ClientEffect
case class SetCamDir(p: V3F) extends ClientEffect
case class SetCamFOV(fov: Float) extends ClientEffect
case object CaptureCursor extends ClientEffect
case object ReleaseCursor extends ClientEffect
case object Exit extends ClientEffect

sealed trait Button
case object Left extends Button
case object Right extends Button
case object Middle extends Button
case object Back extends Button
case object Forward extends Button
object Button {
  def apply(code: Int): Button = code match {
    case Buttons.BACK => Back
    case Buttons.FORWARD => Forward
    case Buttons.LEFT => Left
    case Buttons.RIGHT => Right
    case Buttons.MIDDLE => Middle
  }
}

object ClientLogic {
  type Output = (ClientLogic, Seq[ClientEffect])
  trait Input {
    def isCursorCaught: Boolean
    def camPos: V3F
    def camDir: V3F
    def windowSize: V2I
    def nanoTime: Long
    def toString(keycode: Int): String
  }
}

trait ClientLogic {
  def nothing: Output = (this, Seq.empty)
  def become(replacement: ClientLogic): Output = (replacement, Seq.empty)
  def cause(effects: ClientEffect*): Output = (this, effects)

  def update(world: World, input: Input): Output = nothing

  def tick(world: World, input: Input): Output = nothing

  def keyDown(keycode: Int)(world: World, input: Input): Output = nothing

  def keyUp(keycode: Int)(world: World, input: Input): Output = nothing

  def keyTyped(char: Char)(world: World, input: Input): Output = nothing

  def touchDown(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): Output = nothing

  def touchUp(pos: V2I, pointer: Int, button: Button)(world: World, input: Input): Output = nothing

  def touchDragged(pos: V2I, delta: V2I, pointer: Int)(world: World, input: Input): Output = nothing

  def mouseMoved(pos: V2I, delta: V2I)(world: World, input: Input): Output = nothing

  def scrolled(amount: Int)(world: World, input: Input): Output = nothing

  def render(world: World, input: Input): (HUD, Seq[RenderUnit]) = EmptyHUD -> Seq.empty

  def resize(world: World, input: Input): Output = nothing
}

