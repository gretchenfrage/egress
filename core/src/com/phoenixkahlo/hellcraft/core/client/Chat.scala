package com.phoenixkahlo.hellcraft.core.client

import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import com.phoenixkahlo.hellcraft.core.client.ClientSessionData.{ChunkDebugMode, ShowTasks}
import com.phoenixkahlo.hellcraft.core.entity._
import com.phoenixkahlo.hellcraft.core.{PutEntity, World}
import com.phoenixkahlo.hellcraft.fgraphics._
import com.phoenixkahlo.hellcraft.math._
import org.json.simple.{JSONObject, JSONValue}

import scala.collection.JavaConverters

object Commands {

  val textures: Map[String, SheetTextureID] = Map(
    "stone" -> StoneTID,
    "sand" -> SandTID,
    "dirt" -> DirtTID,
    "brick" -> BrickTID,
    "grass" -> GrassTID,
    "crosshair" -> CrosshairTID,
    "cursor" -> CursorTID,
    "sound" -> SoundTID,
    "sun" -> SunTID,
    "moon" -> MoonTID,
    "phys" -> PhysTID,
    "gray" -> GrayTID,
    "star" -> StarTID,
    "error" -> ErrorTID
  )

  def sounds: Map[String, SoundID] = Map(
    "snap" -> SnapSID
  )

  val spawnable: Map[String, (V3F, JSONObject) => Entity] = Map(
    "sound" -> ((v, j) => SoundCube(
      sounds(j.get("sound").asInstanceOf[String]),
      j.get("freq").asInstanceOf[Number].intValue,
      v, UUID.randomUUID()
    )),
    "glide" -> ((v, j) => {
      val vel = JavaConverters.asScalaBuffer(j.get("vel").asInstanceOf[java.util.List[_]]).map(_.asInstanceOf[Number].floatValue)
      GlideCube(
        V3F(vel(0), vel(1), vel(2)),
        v, UUID.randomUUID()
      )
    }),
    "ghost" -> ((v, j) => GhostCube(v, UUID.randomUUID())),
    "walker" -> ((v, j) => {
      val walk = Option(j.get("walk"))
        .map(_.asInstanceOf[java.util.List[_]])
        .map(JavaConverters.asScalaBuffer(_))
        .map(_.map(_.asInstanceOf[Number].floatValue))
        .map(floats => V3F(floats(0), floats(1), floats(2)))
        .getOrElse(Origin)
      val vel = Option(j.get("vel"))
        .map(_.asInstanceOf[java.util.List[_]])
        .map(JavaConverters.asScalaBuffer(_))
        .map(_.map(_.asInstanceOf[Number].floatValue))
        .map(floats => V3F(floats(0), floats(1), floats(2)))
        .getOrElse(Origin)

      PhysCube(vel, v + (Up * 10), UUID.randomUUID(), walk)
    })
  )

  def print(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    Seq(ClientPrint(j.asInstanceOf[String]))

  def stevie(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    world.rayhit(input.camPos, input.camDir).map(
      v => CauseUpdateEffect(PutEntity(new Cube(
        textures(j.asInstanceOf[String])
        , v, UUID.randomUUID()), UUID.randomUUID())))
      .toSeq

  def spawn(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    world.rayhit(input.camPos, input.camDir).map(
      v => {
        val entity = spawnable(j.asInstanceOf[JSONObject].get("type").asInstanceOf[String])(
          v, j.asInstanceOf[JSONObject])
        CauseUpdateEffect(PutEntity(entity, UUID.randomUUID()))
      }
    ).toSeq

  def showtasks(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    Seq(SetSessionProperty(ShowTasks, j.asInstanceOf[Boolean]))

  def requesttest(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    Seq(CauseUpdateEffect(RequestTester(j.asInstanceOf[String], RNG.uuids(RNG(ThreadLocalRandom.current.nextLong())))))

  def requestchunktest(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] = {
    val c = j.asInstanceOf[java.util.List[java.util.List[Number]]]
    val start = V3I(c.get(0).get(0).intValue, c.get(0).get(1).intValue, c.get(0).get(2).intValue)
    val delta = V3I(c.get(1).get(0).intValue, c.get(1).get(1).intValue, c.get(1).get(2).intValue)
    Seq(
      CauseUpdateEffect(
        (start toAsSeq (start + delta))
          .flatMap(v => RequestTester.chunkHash(
            v, RNG.uuids(RNG(ThreadLocalRandom.current.nextLong()))
          ))
      )
    )
  }

  def queuestats(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    Seq(
      ClientPrint("seq queue size = " + input.executor.sizeSeq),
      ClientPrint("3D queue size = " + input.executor.size3D),
      ClientPrint("2D queue size = " + input.executor.size2D),
      ClientPrint("DB seq queue size = " + input.executor.sizeDBSeq),
      ClientPrint("DB 3D queue size = " + input.executor.sizeDB3D),
      ClientPrint(""),
      ClientPrint("3D queue height = " + input.executor.height3D),
      ClientPrint("2D queue height = " + input.executor.height2D),
      ClientPrint("DB 3D queue height = " + input.executor.heightDB3D),
      ClientPrint("")
    )

  def chunkdebugmode(world: World, input: ClientLogic.Input)(j: AnyRef): Seq[ClientEffect] =
    Seq(SetSessionProperty(ChunkDebugMode, j.asInstanceOf[String]))

  val commands: Map[String, (World, ClientLogic.Input) => (AnyRef => Seq[ClientEffect])] = Map(
    "print" -> print,
    "stevie" -> stevie,
    "spawn" -> spawn,
    "showtasks" -> showtasks,
    "requesttest" -> requesttest,
    "queuestats" -> queuestats,
    "requestchunktest" -> requestchunktest,
    "chunkdebugmode" -> chunkdebugmode
  )

  def apply(command: String, world: World, input: ClientLogic.Input): Seq[ClientEffect] = {
    try {
      if (command.contains(' ')) {
        val name = command.substring(0, command.indexOf(' '))
        val json = command.substring(command.indexOf(' '), command.length)
        println("json = " + json)
        commands.get(name).map(
          cmd => Option(JSONValue.parse(json)).map(
            cmd(world, input)
          ).getOrElse(Seq(ClientPrint("command failed: invalid json")))
        ).getOrElse(Seq(ClientPrint("command failed: invalid name")))
      } else Seq(ClientPrint("command failed: not enough parts"))
    } catch {
      case e: Exception => Seq(ClientPrint("command failed: " + e))
    }
  }
}


case class Chat(messages: Seq[String]) {
  def +(message: String, world: World, input: ClientLogic.Input): (Chat, Seq[ClientEffect]) = {
    if (message.headOption.contains('/')) Chat(messages :+ message) -> Commands(message.tail, world, input)
    else Chat(messages :+ message) -> Seq.empty
  }
}

object Chat {
  val empty = Chat(Seq.empty)
}