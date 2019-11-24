package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.yuiwai.elevator.Elevator._
import com.yuiwai.elevator.ElevatorSystem.BuildingCallback
import com.yuiwai.elevator.Floor.FloorMsg
import com.yuiwai.elevator.Passenger.{PassengerEvent, PassengerMsg}

object Building {
  final case class BuildingSetting(numOfFloors: Int, numOfElevators: Int)
  final case class BuildingProps(
    setting: BuildingSetting,
    system: ActorRef[BuildingCallback],
    logic: ElevatorLogic
  )
  final case class BuildingState(floors: Map[Int, ActorRef[FloorMsg]], elevators: Set[ActorRef[ElevatorMsg]])

  sealed trait BuildingEvent {
    def callback: BuildingCallback = BuildingCallback(this)
  }
  case object BuildingInitialized extends BuildingEvent
  case object Updated extends BuildingEvent

  sealed trait BuildingMsg
  final case class Tick(time: Int) extends BuildingMsg
  final case class Join(passenger: ActorRef[PassengerMsg], floor: Int, direction: Direction) extends BuildingMsg

  final case class ElevatorCallback(event: ElevatorEvent) extends BuildingMsg
  final case class PassengerCallback(event: PassengerEvent) extends BuildingMsg

  def apply(props: BuildingProps): Behavior[BuildingMsg] = Behaviors.setup { ctx =>
    (1 to props.setting.numOfElevators).map { i =>
      ctx.spawn(Elevator(ElevatorProps(ctx.self)), s"elevator-$i")
    }
    initializing(BuildingState(Map(), Set()))(props)
  }

  private[elevator] def initializing(state: BuildingState)
    (implicit props: BuildingProps): Behavior[BuildingMsg] =
    if (state.elevators.size >= props.setting.numOfElevators) {
      props.system ! BuildingCallback(BuildingInitialized)
      progressing(state)
    } else Behaviors.receiveMessage {
      case ElevatorCallback(ElevatorInitialized(elevator)) =>
        initializing(state.copy(elevators = state.elevators + elevator))
    }

  private[elevator] def progressing(state: BuildingState, numOfExecutedElevators: Int = 0)
    (implicit props: BuildingProps): Behavior[BuildingMsg] = Behaviors.receiveMessage {
    case Join(passenger, floor, direction) =>
      props.logic.join(state, floor, direction)
      Behaviors.same
    case Tick(time) =>
      state.elevators.foreach(_ ! Execute)
      Behaviors.same

    case ElevatorCallback(event) => event match {
      case StateKept(stateType) =>
        val count = numOfExecutedElevators + 1
        if (count >= state.elevators.size) {
          // TODO Passengerの更新後にUpdatedを返すように
          props.system ! Updated.callback
          Behaviors.same
        }
        else progressing(state, count)
    }
    case PassengerCallback(event) =>
      // TODO イベントハンドリング
      Behaviors.same
  }
}
