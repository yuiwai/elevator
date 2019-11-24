package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.yuiwai.elevator.Building.{BuildingSetting, PassengerCallback}
import com.yuiwai.elevator.Elevator.{Direction, ElevatorMsg, Enter, Ignore}
import com.yuiwai.elevator.ElevatorSystem.PassengerGeneratorCallback

object Passenger {
  final case class PassengerProps(building: ActorRef[PassengerCallback], interval: Int, createdAt: Int)
  final case class PassengerState(destination: Int)

  sealed trait PassengerEvent {
    def callback: PassengerCallback = PassengerCallback(this)
  }
  final case class Appeared(floor: Int, destination: Int) extends PassengerEvent
  final case class Left(createdAt: Int) extends PassengerEvent
  final case class Waiting() extends PassengerEvent
  final case class Boarding() extends PassengerEvent
  final case class Staying() extends PassengerEvent

  sealed trait PassengerMsg
  final case class Arrived(elevator: ActorRef[ElevatorMsg], floor: Int, direction: Option[Direction]) extends PassengerMsg
  final case class Stopped(elevator: ActorRef[ElevatorMsg], floor: Int) extends PassengerMsg
  final case class TickToPassenger(time: Int) extends PassengerMsg

  def apply(props: PassengerProps, floor: Int, destination: Int): Behavior[PassengerMsg] = {
    props.building ! Appeared(floor, destination).callback
    waiting(PassengerState(destination), floor)(props)
  }
  private[elevator] def waiting(state: PassengerState, currentFloor: Int)
    (implicit props: PassengerProps): Behavior[PassengerMsg] = Behaviors.setup { ctx =>
    Behaviors.receiveMessage {
      case Arrived(elevator, floor, direction) =>
        Direction(currentFloor, state.destination) match {
          // 最初から目的階にいる場合、stayさせない
          case None =>
            props.building ! Left(props.createdAt).callback
            Behaviors.stopped
          case Some(d) =>
            if (currentFloor == floor && direction.forall(_ == d)) {
              elevator ! Enter(ctx.self, state.destination)
              boarding()
            } else {
              elevator ! Ignore(ctx.self)
              Behaviors.same
            }
        }
    }
  }
  private[elevator] def boarding(): Behavior[PassengerMsg] = Behaviors.setup { ctx =>
    Behaviors.receiveMessage {
      case _ => Behaviors.same
    }
  }
  private[elevator] def staying(state: PassengerState, stayingFrom: Int)
    (implicit props: PassengerProps): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case TickToPassenger(time) =>
      if (stayingFrom + props.interval >= time) {
        props.building ! Staying().callback
        Behaviors.same
      } else {
        props.building ! Waiting().callback
        waiting(PassengerState(1), state.destination)
      }
  }
}

object PassengerGenerator {
  final case class PassengerGeneratorProps(
    seed: Int,
    setting: BuildingSetting,
    system: ActorRef[PassengerGeneratorCallback])
  sealed trait PassengerGeneratorEvent {
    def callback: PassengerGeneratorCallback = PassengerGeneratorCallback(this)
  }
  final case class PassengerGenerated(destination: Int, interval: Int) extends PassengerGeneratorEvent

  sealed trait PassengerGeneratorMsg
  sealed trait PassengerGeneratorCommand extends PassengerGeneratorMsg
  final case class GeneratePassenger(time: Int) extends PassengerGeneratorCommand

  def apply(props: PassengerGeneratorProps): Behavior[PassengerGeneratorMsg] = Behaviors.receiveMessage {
    // TODO 毎回生成されてしまうので、はずれのケースを設けてOptionで返すように
    case GeneratePassenger(time) =>
      import RandomUtil._
      props.system ! PassengerGenerated(
        nextInt(props.seed + time, 10),
        nextInt(props.seed + time * 2, 5)).callback
      Behaviors.same
  }
}
