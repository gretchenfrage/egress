package com.phoenixkahlo.hellcraft.core.client

import com.badlogic.gdx.Input.Buttons
import com.phoenixkahlo.hellcraft.core.UpdateEffect
import com.phoenixkahlo.hellcraft.core.client.ClientLogic.{Input, Output, RenderOutput}
import com.phoenixkahlo.hellcraft.core.client.ClientSessionData.ClientSessionData
import com.phoenixkahlo.hellcraft.core.graphics.RenderWorld
import com.phoenixkahlo.hellcraft.fgraphics.{GlobalRenderData, Render, ResourcePack, Shader}
import com.phoenixkahlo.hellcraft.math.{V2F, V2I, V3F, V3I}
import com.phoenixkahlo.hellcraft.util.collections.TypeMatchingMap.Default
import com.phoenixkahlo.hellcraft.util.collections.{Identity, TypeMatchingMap, V3ISet}
import com.phoenixkahlo.hellcraft.util.threading.UniExecutor

import scala.concurrent.duration._

sealed trait ClientEffect
case class CauseUpdateEffect(effects: Seq[UpdateEffect]) extends ClientEffect
object CauseUpdateEffect {
  def apply(effect: UpdateEffect): CauseUpdateEffect = CauseUpdateEffect(Seq(effect))
}
case class SetLoadTarget(target: V3ISet, terrains: V3ISet) extends ClientEffect
case object CaptureCursor extends ClientEffect
case object ReleaseCursor extends ClientEffect
case object Exit extends ClientEffect
case class ClientPrint(str: String) extends ClientEffect
case class SetSessionProperty[T](k: ClientSessionData.Field[T], v: T) extends ClientEffect

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

object ClientSessionData {
  sealed class Field[T](val default: Option[T])
  case object ChunkDebugMode extends Field[String](Some(""))
  case object ShowTasks extends Field[Boolean](Some(false))
  case object Sensitivity extends Field[Float](Some(0.25f))
  case object Speed extends Field[Float](Some(50))
  case object LoadDist extends Field[V3I](Some(V3I(12, 5, 12)))
  case object DayCycle extends Field[Duration](Some(3 minutes))
  case object CloudSpeed extends Field[Float](Some(10))
  case object JumpHeight extends Field[Float](Some(1.2f))
  type ClientSessionData = TypeMatchingMap[Field, Identity, Any]
  val empty: ClientSessionData = TypeMatchingMap.empty[Field, Identity, Any].withDefault(
    new Default[Field, Identity, Any] {
      override def apply[T](k: Field[T]): Option[T] = k.default
    })
}

object ClientLogic {
  type Output = (ClientLogic, Seq[ClientEffect])
  type RenderOutput = (Seq[Render[_ <: Shader]], GlobalRenderData)
  trait Input {
    def isCursorCaught: Boolean
    def camPos: V3F
    def camDir: V3F
    def currentRes: V2I
    def nanoTime: Long
    def keyToChar(keycode: Int): Option[Char]
    def sessionData: ClientSessionData
    def pack: ResourcePack
    def executor: UniExecutor
    def cursorPos: V2I
    def camRange: (Float, Float)
    def dt: Float
  }
}

trait ClientLogic {
  def nothing: Output = (this, Seq.empty)
  def become(replacement: ClientLogic): Output = (replacement, Seq.empty)
  def cause(effects: ClientEffect*): Output = (this, effects)

  def frame(world: ClientRenderWorld, input: Input): (Output, RenderOutput)

  def tick(world: ClientWorld, input: Input): Output = nothing

  def keyDown(keycode: Int)(world: ClientWorld, input: Input): Output = nothing

  def keyUp(keycode: Int)(world: ClientWorld, input: Input): Output = nothing

  def keyTyped(char: Char)(world: ClientWorld, input: Input): Output = nothing

  def touchDown(pos: V2I, pointer: Int, button: Button)(world: ClientWorld, input: Input): Output = nothing

  def touchUp(pos: V2I, pointer: Int, button: Button)(world: ClientWorld, input: Input): Output = nothing

  def touchDragged(pos: V2I, delta: V2I, pointer: Int)(world: ClientWorld, input: Input): Output = mouseMoved(pos, delta)(world, input)

  def mouseMoved(pos: V2I, delta: V2I)(world: ClientWorld, input: Input): Output = nothing

  def scrolled(amount: Int)(world: ClientWorld, input: Input): Output = nothing

  def resize(world: ClientWorld, input: Input): Output = nothing
}

