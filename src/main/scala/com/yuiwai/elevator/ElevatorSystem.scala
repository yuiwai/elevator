package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.yuiwai.elevator.Building._
import com.yuiwai.elevator.Elevator.{Direction, ElevatorEvent, ElevatorInitialized, ElevatorMsg, ElevatorProps}
import com.yuiwai.elevator.ElevatorSystem.{BuildingCallback, PassengerGeneratorCallback}
import com.yuiwai.elevator.Passenger.{PassengerEvent, PassengerMsg, PassengerProps}
import com.yuiwai.elevator.PassengerGenerator.{apply => _, _}

import scala.io.StdIn
import scala.util.Random

object EntryPoint {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ElevatorSystem(BuildingSetting(10, 2)), "elevator-system")
    StdIn.readLine()
    system.terminate()
  }
}

object ElevatorSystem {
  final case class ElevatorSystemProps(
    building: ActorRef[BuildingMsg],
    generator: ActorRef[PassengerGeneratorMsg]
  )
  final case class ElevatorSystemState(currentTime: Int, passengers: Set[ActorRef[PassengerMsg]])

  sealed trait ElevatorSystemMsg
  final case class BuildingCallback(event: BuildingEvent) extends ElevatorSystemMsg

  final case class PassengerGeneratorCallback(event: PassengerGeneratorEvent) extends ElevatorSystemMsg

  def apply(setting: BuildingSetting, seed: Long = 1): Behavior[ElevatorSystemMsg] = Behaviors.setup { ctx =>
    Random.setSeed(seed)
    implicit val elevatorSystemProps: ElevatorSystemProps = ElevatorSystemProps(
      ctx.spawn(Building(BuildingProps(setting, ctx.self)), "building"),
      ctx.spawn(PassengerGenerator(PassengerGeneratorProps(Random.nextInt, setting, ctx.self)), "passenger-generator")
    )
    initializing(ElevatorSystemState(0, Set.empty))
  }

  private def initializing(state: ElevatorSystemState)(implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] =
    Behaviors.receiveMessage {
      case BuildingCallback(BuildingInitialized) => processing(state)
    }

  private def processing(state: ElevatorSystemState)
    (implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] =
    Behaviors.setup { ctx =>
      props.generator ! GeneratePassenger(state.currentTime)
      Behaviors.receiveMessage {
        case PassengerGeneratorCallback(event) =>
          event match {
            case PassengerGenerated(destination, interval) =>
              // TODO 現状は1ターンに最大1人ずつ生成
              val passenger = ctx.spawn(
                Passenger(PassengerProps(destination, interval, state.currentTime), 1),
                s"passenger-${state.currentTime}")
              props.building ! Join(passenger, 1)
              processing(state.copy(passengers = state.passengers + passenger))
          }
        case _ => Behaviors.same
      }
    }
}

object Building {
  final case class BuildingSetting(numOfFloors: Int, numOfElevators: Int)
  final case class BuildingProps(
    setting: BuildingSetting,
    system: ActorRef[BuildingCallback]
  )
  final case class BuildingState(elevators: Set[ActorRef[ElevatorMsg]])

  sealed trait BuildingEvent {
    def callback: BuildingCallback = BuildingCallback(this)
  }
  case object BuildingInitialized extends BuildingEvent

  sealed trait BuildingMsg
  final case class Tick(time: Int) extends BuildingMsg
  final case class Join(passenger: ActorRef[PassengerMsg], floor: Int) extends BuildingMsg

  final case class ElevatorCallback(event: ElevatorEvent) extends BuildingMsg
  final case class PassengerCallback(event: PassengerEvent) extends BuildingMsg

  def apply(props: BuildingProps): Behavior[BuildingMsg] = Behaviors.setup { ctx =>
    (1 to props.setting.numOfElevators).map { i =>
      ctx.spawn(Elevator(ElevatorProps(ctx.self)), s"elevator-$i")
    }
    initializing(BuildingState(Set()))(props)
  }

  private[elevator] def initializing(state: BuildingState)
    (implicit props: BuildingProps): Behavior[BuildingMsg] =
    if (state.elevators.size >= props.setting.numOfElevators) {
      props.system ! BuildingCallback(BuildingInitialized)
      progressing(state)
    } else Behaviors.receiveMessage {
      case ElevatorCallback(ElevatorInitialized(elevator)) =>
        initializing(state.copy(elevators = state.elevators + elevator))
      case _ => Behaviors.same
    }

  private[elevator] def progressing(state: BuildingState)
    (implicit props: BuildingProps): Behavior[BuildingMsg] = Behaviors.receive {
    case _ => Behaviors.same
  }
}

object Elevator {
  final case class ElevatorProps(building: ActorRef[ElevatorCallback])
  final case class ElevatorState(currentFloor: Int, targetFloor: Option[Int])

  sealed trait ElevatorStateType
  case object Stopping extends ElevatorStateType
  case object Moving extends ElevatorStateType

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction

  sealed trait ElevatorEvent {
    def callback: ElevatorCallback = ElevatorCallback(this)
  }
  final case class ElevatorInitialized(elevator: ActorRef[ElevatorMsg]) extends ElevatorEvent
  final case class Entered(passenger: ActorRef[PassengerMsg]) extends ElevatorEvent
  final case class StateKept(stateType: ElevatorStateType) extends ElevatorEvent
  final case class StateChanged(stateType: ElevatorStateType) extends ElevatorEvent

  sealed trait ElevatorError extends ElevatorEvent
  final case class EnterError(passenger: ActorRef[PassengerMsg]) extends ElevatorError

  sealed trait ElevatorMsg
  final case class Enter(passenger: ActorRef[PassengerMsg]) extends ElevatorMsg
  case object Execute extends ElevatorMsg

  def apply(props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.setup { ctx =>
    props.building ! ElevatorInitialized(ctx.self).callback
    stopping(ElevatorState(1, None))(props)
  }

  private[elevator] def stopping(state: ElevatorState)
    (implicit props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.receiveMessage {
    case Enter(passenger) =>
      props.building ! Entered(passenger).callback
      Behaviors.same
    case Execute =>
      state.targetFloor match {
        case Some(_) =>
          props.building ! StateChanged(Moving).callback
          moving(state)
        case None =>
          props.building ! StateKept(Stopping).callback
          Behaviors.same
      }
  }

  private[elevator] def moving(state: ElevatorState)
    (implicit props: ElevatorProps): Behavior[ElevatorMsg] = Behaviors.receiveMessage {
    case Enter(passenger) =>
      props.building ! EnterError(passenger).callback
      Behaviors.same
    case _ => Behaviors.same
  }
}

object Passenger {
  final case class PassengerProps(destination: Int, interval: Int, createdAt: Int)
  final case class PassengerState()

  sealed trait PassengerEvent
  final case class Appeared(floor: Int, destination: Int) extends PassengerEvent
  final case class Left(createdAt: Int) extends PassengerEvent

  sealed trait PassengerMsg
  final case class Arrived(floor: Int, direction: Option[Direction]) extends PassengerMsg
  final case class Stopped(floor: Int) extends PassengerMsg

  def apply(props: PassengerProps, floor: Int): Behavior[PassengerMsg] =
    Behaviors.setup(_ => waiting(floor))
  private def waiting(currentFloor: Int): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
  }
  private def staying(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
  }
  private def boarding(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
    case _ => Behaviors.same
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
    case GeneratePassenger(time) =>
      import RandomUtil._
      props.system ! PassengerGenerated(
        nextInt(props.seed + time, 10),
        nextInt(props.seed + time * 2, 5)).callback
      Behaviors.same
  }
}

object RandomUtil {
  def nextDouble(x: Int, range: Int): Double = next(x).toDouble / Int.MaxValue
  def nextInt(x: Int, range: Int): Int = (next(x).toDouble / Int.MaxValue * range).toInt.abs
  def next(x: Int): Int = {
    var y: Int = x
    y = y ^ (y << 13)
    y = y ^ (y >> 17)
    y ^ (y << 15)
  }
}


trait ElevatorLogic {

}