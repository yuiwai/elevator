package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.yuiwai.elevator.Building._
import com.yuiwai.elevator.Elevator.{apply => _, _}
import com.yuiwai.elevator.ElevatorSystem.{BuildingCallback, PassengerGeneratorCallback}
import com.yuiwai.elevator.Passenger.{PassengerEvent, PassengerMsg, PassengerProps}
import com.yuiwai.elevator.PassengerGenerator.{apply => _, _}

import scala.io.StdIn
import scala.util.Random
import scala.util.chaining._

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
      ctx.spawn(Building(BuildingProps(setting, ctx.self, DefaultLogic)), "building"),
      ctx.spawn(PassengerGenerator(PassengerGeneratorProps(Random.nextInt, setting, ctx.self)), "passenger-generator")
    )
    initializing(ElevatorSystemState(0, Set.empty))
  }

  private def initializing(state: ElevatorSystemState)
    (implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] =
    Behaviors.receiveMessage {
      case BuildingCallback(BuildingInitialized) => beforeTurn(state)
    }

  private[elevator] def beforeTurn(state: ElevatorSystemState)
    (implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] = Behaviors.setup { ctx =>
    props.generator ! GeneratePassenger(state.currentTime)
    Behaviors.receiveMessage {
      case PassengerGeneratorCallback(event) =>
        event match {
          case PassengerGenerated(destination, interval) =>
            // TODO 現状は1ターンに最大1人ずつ生成
            val passenger = ctx.spawn(
              Passenger(PassengerProps(props.building, interval, state.currentTime), 1, destination),
              s"passenger-${state.currentTime}")
            Direction(1, destination) foreach (direction => props.building ! Join(passenger, 1, direction))
            processTurn(state.copy(passengers = state.passengers + passenger))
        }
    }
  }

  private[elevator] def processTurn(state: ElevatorSystemState)
    (implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] = Behaviors.setup { ctx =>
    props.building ! Tick(state.currentTime)
    Behaviors.receiveMessage {
      case BuildingCallback(event) =>
        event match {
          case Updated => afterTurn(state)
        }
    }
  }

  private[elevator] def afterTurn(state: ElevatorSystemState): Behavior[ElevatorSystemMsg] = ???
}

object Building {
  final case class BuildingSetting(numOfFloors: Int, numOfElevators: Int)
  final case class BuildingProps(
    setting: BuildingSetting,
    system: ActorRef[BuildingCallback],
    logic: ElevatorLogic
  )
  final case class BuildingState(elevators: Set[ActorRef[ElevatorMsg]])

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
          props.system ! Updated.callback
          Behaviors.same
        }
        else progressing(state, count)
    }
  }
}

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
  private[elevator] def boarding(): Behavior[PassengerMsg] = Behaviors.receiveMessage {
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
    // TODO 毎回生成されてしまうので、はずれのケースを設けてOptionで返すように
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
  def join(state: BuildingState, floor: Int, direction: Direction): BuildingState
}
object DefaultLogic extends ElevatorLogic {
  def join(state: BuildingState, floor: Int, direction: Direction): BuildingState = state
}
