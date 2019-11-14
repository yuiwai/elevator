package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}

import scala.util.Random

object ElevatorSystem {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem(apply(), "elevator-system")
  }

  sealed trait SystemMsg
  def apply(seed: Long = 1): Behavior[SystemMsg] = Behaviors.setup { ctx =>
    Random.setSeed(seed)
    val building = ctx.spawn(Building.apply(1), "building")
    Behaviors.receiveMessage {
      case _ => Behaviors.same
    }
  }
}

object Building {
  sealed trait BuildingMsg
  final case class Tick(time: Long)
  def apply(numOfElevators: Int): Behavior[BuildingMsg] = Behaviors.setup { ctx =>
    val elevators = (1 to numOfElevators).map { i =>
      ctx.spawn(Elevator.apply(), s"elevator_$i")
    }
    Behaviors.receiveMessage {
      case _ => Behaviors.same
    }
  }
}

object Elevator {
  sealed trait ElevatorMsg
  def apply(currentFloor: Int = 1, targetFloor: Option[Int] = None): Behavior[ElevatorMsg] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case _ => Behaviors.same
      }
    }
}

object Passenger {
  sealed trait PassengerMsg
  def apply(goto: Int, wait: Int): Behavior[PassengerMsg] = Behaviors.receive { (ctx, msg) =>
    msg match {
      case _ => Behaviors.same
    }
  }
}
