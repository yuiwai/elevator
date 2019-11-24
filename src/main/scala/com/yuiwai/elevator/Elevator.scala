package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.yuiwai.elevator.Building.ElevatorCallback
import com.yuiwai.elevator.Passenger.PassengerMsg

import scala.util.chaining._

object Elevator {
  final case class ElevatorProps(building: ActorRef[ElevatorCallback])
  final case class ElevatorState(currentFloor: Int, targetFloors: List[Int])

  sealed trait ElevatorStateType
  case object Stopping extends ElevatorStateType
  case object Moving extends ElevatorStateType

  sealed trait Direction
  object Direction {
    def apply(from: Int, to: Int): Option[Direction] =
      if (from == to) None
      else if (from > to) Some(Down)
      else Some(Up)
  }
  case object Up extends Direction
  case object Down extends Direction

  sealed trait ElevatorEvent {
    def callback: ElevatorCallback = ElevatorCallback(this)
  }
  final case class ElevatorInitialized(elevator: ActorRef[ElevatorMsg]) extends ElevatorEvent
  final case class Entered(passenger: ActorRef[PassengerMsg]) extends ElevatorEvent
  final case class StateKept(stateType: ElevatorStateType) extends ElevatorEvent
  final case class StateChanged(stateType: ElevatorStateType) extends ElevatorEvent
  final case class StopAdded(floors: List[Int]) extends ElevatorEvent

  sealed trait ElevatorError extends ElevatorEvent
  final case class EnterError(passenger: ActorRef[PassengerMsg]) extends ElevatorError

  sealed trait ElevatorMsg
  final case class Enter(passenger: ActorRef[PassengerMsg], destination: Int) extends ElevatorMsg
  final case class Ignore(passenger: ActorRef[PassengerMsg]) extends ElevatorMsg
  final case class Left(passenger: ActorRef[PassengerMsg]) extends ElevatorMsg
  final case class AddStop(floor: Int) extends ElevatorMsg
  case object Execute extends ElevatorMsg

  private def addStopImpl(current: Int, add: Int, before: List[Int], after: List[Int]): List[Int] = after match {
    case Nil => before :+ add
    case h :: t =>
      if (h == add) before ++ after
      else if (h.max(current) > add && h.min(current) < add) before ++ (add :: after)
      else addStopImpl(current, add, before :+ h, t)
  }
  private def addStop(state: ElevatorState, floor: Int)
    (implicit props: ElevatorProps): ElevatorState = {
    (state.targetFloors match {
      // 行き先が空なら単に追加
      case Nil => state.copy(targetFloors = floor :: Nil)
      // 行き先が空でなければ次の行き先と現在位置との間に挿入
      case l => state.copy(targetFloors = addStopImpl(state.currentFloor, floor, Nil, l))
    })
      .tap { s =>
        props.building ! StopAdded(s.targetFloors).callback
      }
  }

  def apply(props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.setup { ctx =>
    props.building ! ElevatorInitialized(ctx.self).callback
    stopping(ElevatorState(1, Nil))(props)
  }

  private[elevator] def stopping(state: ElevatorState)
    (implicit props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.receiveMessage {
    case Enter(passenger, destination) =>
      props.building ! Entered(passenger).callback
      stopping(addStop(state, destination))
    case AddStop(floor) =>
      stopping(addStop(state, floor))
    case Execute =>
      state.targetFloors match {
        case Nil =>
          props.building ! StateKept(Stopping).callback
          Behaviors.same
        case _ =>
          props.building ! StateChanged(Moving).callback
          moving(state)
      }
  }

  private[elevator] def moving(state: ElevatorState)
    (implicit props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.receiveMessage {
    case Enter(passenger, _) =>
      props.building ! EnterError(passenger).callback
      Behaviors.same
    case AddStop(floor) =>
      moving(addStop(state, floor))
  }
}
