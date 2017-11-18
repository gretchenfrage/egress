package com.phoenixkahlo.hellcraft.core.client

case class Chat(messages: Seq[String]) {

  def +(message: String): Chat = Chat(messages :+ message)

}

object Chat {
  val empty = Chat(Seq.empty)
}