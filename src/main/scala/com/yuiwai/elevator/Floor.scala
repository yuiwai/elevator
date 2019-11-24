package com.yuiwai.elevator

import akka.actor.typed.Behavior

object Floor {
  sealed trait FloorMsg
  final case class JoinPassenger() extends FloorMsg

  def apply(): Behavior[FloorMsg] = ???

  private def initialize(): Behavior[FloorMsg] = ???

  private def processing(): Behavior[FloorMsg] = ???
}
