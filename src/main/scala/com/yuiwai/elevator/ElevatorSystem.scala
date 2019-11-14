package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.yuiwai.elevator.Building.{BuildingEvent, BuildingInitialized, BuildingState}
import com.yuiwai.elevator.Elevator.{ElevatorEvent, ElevatorInitialized}
import com.yuiwai.elevator.ElevatorSystem.BuildingCallback
import com.yuiwai.elevator.Passenger.PassengerMsg

import scala.util.Random

object EntryPoint {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ElevatorSystem(), "elevator-system")
  }
}

object ElevatorSystem {
  sealed trait SystemMsg
  final case class BuildingCallback(event: BuildingEvent) extends SystemMsg

  sealed trait PassengerGeneratorEvent extends SystemMsg
  final case class PassengerGenerated(passengers: Seq[ActorRef[PassengerMsg]]) extends PassengerGeneratorEvent

  def apply(seed: Long = 1): Behavior[SystemMsg] = Behaviors.setup { ctx =>
    Random.setSeed(seed)
    val building = ctx.spawn(Building(BuildingState(1, ctx.self)), "building")
    Behaviors.receiveMessage {
      case BuildingCallback(event) =>
        event match {
          case BuildingInitialized => Behaviors.same
        }
      case _ => Behaviors.same
    }
  }
}

object Building {
  final case class BuildingState(
    numOfElevators: Int,
    system: ActorRef[BuildingCallback]
  )

  sealed trait BuildingEvent
  case object BuildingInitialized extends BuildingEvent

  sealed trait BuildingMsg
  final case class Tick(time: Long) extends BuildingMsg

  final case class ElevatorCallback(event: ElevatorEvent) extends BuildingMsg

  def apply(state: BuildingState): Behavior[BuildingMsg] = Behaviors.setup { ctx =>
    val elevators = (1 to state.numOfElevators).map { i =>
      ctx.spawn(Elevator.apply(), s"elevator_$i")
    }
    state.system ! BuildingCallback(BuildingInitialized)
    Behaviors.receiveMessage {
      case ElevatorCallback(event) =>
        event match {
          case ElevatorInitialized => Behaviors.same
        }
      case _ => Behaviors.same
    }
  }
}

object Elevator {
  final case class ElevatorState()
  sealed trait ElevatorEvent
  case object ElevatorInitialized extends ElevatorEvent

  sealed trait ElevatorMsg
  final case class Enter(passenger: ActorRef[PassengerMsg]) extends ElevatorMsg
  def apply(currentFloor: Int = 1, targetFloor: Option[Int] = None): Behavior[ElevatorMsg] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case _ => Behaviors.same
      }
    }
}

object Passenger {
  final case class PassengerState()

  sealed trait PassengerEvent
  case object LeftBuilding extends PassengerEvent

  sealed trait PassengerMsg
  final case class Arrived(floor: Int) extends PassengerMsg
  def apply(destination: Int, interval: Int, createdAt: Long): Behavior[PassengerMsg] =
    Behaviors.setup(_ => waiting())
  private def waiting(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
  }
  private def stay(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
  }
  private def back(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
  }
}

object PassengerGenerator {
  sealed trait PassengerGeneratorMsg
  sealed trait PassengerGeneratorCommand
  final case class GeneratePassenger(time: Long) extends PassengerGeneratorCommand

  def apply(): Behavior[PassengerGeneratorMsg] = Behaviors.receive {
    (_, _) => Behaviors.same
  }
}
