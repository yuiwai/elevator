package com.yuiwai.elevator

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.yuiwai.elevator.Building._
import com.yuiwai.elevator.Elevator.{apply => _, _}
import com.yuiwai.elevator.Passenger.{PassengerMsg, PassengerProps}
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
    generator: ActorRef[PassengerGeneratorMsg],
    maxTime: Int
  )
  final case class ElevatorSystemState(currentTime: Int, passengers: Set[ActorRef[PassengerMsg]])

  sealed trait ElevatorSystemMsg
  final case class BuildingCallback(event: BuildingEvent) extends ElevatorSystemMsg

  final case class PassengerGeneratorCallback(event: PassengerGeneratorEvent) extends ElevatorSystemMsg

  def apply(setting: BuildingSetting, seed: Long = 1): Behavior[ElevatorSystemMsg] = Behaviors.setup { ctx =>
    Random.setSeed(seed)
    implicit val elevatorSystemProps: ElevatorSystemProps = ElevatorSystemProps(
      ctx.spawn(Building(BuildingProps(setting, ctx.self, DefaultLogic)), "building"),
      ctx.spawn(PassengerGenerator(PassengerGeneratorProps(Random.nextInt, setting, ctx.self)), "passenger-generator"),
      1000
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
      case BuildingCallback(event) =>
        // TODO イベントハンドリング
        Behaviors.same
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

  private[elevator] def afterTurn(state: ElevatorSystemState)
    (implicit props: ElevatorSystemProps): Behavior[ElevatorSystemMsg] =
    state.currentTime + 1 match {
      case nextTime =>
        if (nextTime < props.maxTime) beforeTurn(state.copy(currentTime = nextTime))
        else Behaviors.stopped
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
