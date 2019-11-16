package com.yuiwai.elevator

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import com.yuiwai.elevator.Building._
import com.yuiwai.elevator.Elevator.{apply => _, _}
import com.yuiwai.elevator.ElevatorSystem.{BuildingCallback, PassengerGeneratorCallback}
import com.yuiwai.elevator.Passenger.{Arrived, PassengerMsg, PassengerProps}
import com.yuiwai.elevator.PassengerGenerator.{GeneratePassenger, PassengerGenerated, PassengerGeneratorProps}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class ElevatorSystemSpec {
}

class BuildingSpec extends WordSpec with BeforeAndAfterAll with Matchers {
  val testKit = ActorTestKit()
  override def afterAll(): Unit = testKit.shutdownTestKit()

  "ビルは" should {
    "初期化時にエレベーターも初期化される" in {
      val system = testKit.createTestProbe[BuildingCallback]()
      testKit.spawn(Building(BuildingProps(BuildingSetting(10, 2), system.ref)))
      system.expectMessage(BuildingInitialized.callback)
    }
  }
  "初期化済みのビルは" should {
    "乗客を参加させることが出来る" in {
      val system = testKit.createTestProbe[BuildingCallback]()
      val passenger = testKit.createTestProbe[PassengerMsg]()
      val building = testKit.spawn(Building.progressing(BuildingState(Set.empty))(BuildingProps(BuildingSetting(10, 2), system.ref)))
      building ! Join(passenger.ref, 1)
    }
  }
}

class ElevatorSpec extends WordSpec with BeforeAndAfterAll with Matchers {
  val testKit = ActorTestKit()
  override def afterAll(): Unit = testKit.shutdownTestKit()

  "止まっているエレベーターは" should {
    "乗客が乗り込むことが出来る" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val passenger = testKit.createTestProbe[Passenger.PassengerMsg]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, None))(ElevatorProps(building.ref)))
      elevator ! Enter(passenger.ref)
      building.expectMessage(Entered(passenger.ref).callback)
    }
    "行き先未指定の場合は動かない" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, None))(Elevator.ElevatorProps(building.ref)))
      elevator ! Execute
      building.expectMessage(StateKept(Stopping).callback)
    }
    "行き先の方向に動き出す" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, Some(2)))(Elevator.ElevatorProps(building.ref)))
      elevator ! Execute
      building.expectMessage(StateChanged(Moving).callback)
    }
  }

  "動いているエレベーターは" should {
    "乗客が乗り込めない" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val passenger = testKit.createTestProbe[Passenger.PassengerMsg]()
      val elevator = testKit.spawn(Elevator.moving(ElevatorState(1, None))(Elevator.ElevatorProps(building.ref)))
      elevator ! Enter(passenger.ref)
      building.expectMessage(EnterError(passenger.ref).callback)
    }
  }
}

class PassengerSpec extends WordSpec with BeforeAndAfterAll with Matchers {
  val testKit = ActorTestKit()
  override def afterAll(): Unit = testKit.shutdownTestKit()

  "エレベータを待っている乗客は" should {
    "行き先の方向が一致したエレベーターに乗る" in {
      val passenger = testKit.spawn(Passenger(PassengerProps(5, 5, 1), 1))
      passenger ! Arrived(1)
    }
  }
}

class PassengerGeneratorSpec extends WordSpec {
  val testKit = ActorTestKit()
  "PassengerGeneratorは" should {
    "行き先と滞在時間を生成する" in {
      val system = testKit.createTestProbe[PassengerGeneratorCallback]()
      val generator = testKit.spawn(PassengerGenerator(PassengerGeneratorProps(1, BuildingSetting(10, 2), system.ref)))
      generator ! GeneratePassenger(1)
      system.expectMessage(PassengerGenerated(2, 1).callback)
    }
  }
}
