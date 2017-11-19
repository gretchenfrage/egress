package com.phoenixkahlo.hellcraft.core.client

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.{JString, JValue}

object Commands {
  import net.liftweb.json._
  implicit val formats = DefaultFormats

  def print(j: JValue): Seq[ClientEffect] =
    Seq(ClientPrint(j.asInstanceOf[JString].s))

  val commands: Map[String, JValue => Seq[ClientEffect]] = Map(
    "print" -> print
  )

  def apply(command: String): Seq[ClientEffect] = {
    try {
      if (command.contains(' ')) {
        val name = command.substring(0, command.indexOf(' '))
        val json = command.substring(command.indexOf(' '), command.length)
        commands.get(name).map(
          command => parseOpt(json).map(
            command
          ).getOrElse(Seq(ClientPrint("command failed: invalid json")))
        ).getOrElse(Seq(ClientPrint("command failed: invalid name")))
      } else Seq(ClientPrint("command failed: not enough parts"))
    } catch {
      case e: Exception => Seq(ClientPrint("command failed: " + e))
    }
  }
}


case class Chat(messages: Seq[String]) {
  def +(message: String): (Chat, Seq[ClientEffect]) = {
    if (message.headOption.contains('/')) Chat(messages :+ message) -> Commands(message.tail)
    else Chat(messages :+ message) -> Seq.empty
  }
}

object Chat {
  val empty = Chat(Seq.empty)
}