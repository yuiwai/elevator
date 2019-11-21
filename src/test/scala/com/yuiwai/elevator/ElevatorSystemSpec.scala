package com.yuiwai.elevator

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import com.yuiwai.elevator.Building._
import com.yuiwai.elevator.Elevator.{apply => _, _}
import com.yuiwai.elevator.ElevatorSystem.{BuildingCallback, PassengerGeneratorCallback}
import com.yuiwai.elevator.Passenger.{Arrived, PassengerMsg, PassengerProps, PassengerState, Staying, TickToPassenger, Waiting}
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
      testKit.spawn(Building(BuildingProps(BuildingSetting(10, 2), system.ref, DefaultLogic)))
      system.expectMessage(BuildingInitialized.callback)
    }
  }
  "初期化済みのビルは" should {
    "乗客を参加させることが出来る" in {
      val system = testKit.createTestProbe[BuildingCallback]()
      val passenger = testKit.createTestProbe[PassengerMsg]()
      val building = testKit.spawn(Building.progressing(BuildingState(Set.empty))(BuildingProps(BuildingSetting(10, 2), system.ref, DefaultLogic)))
      building ! Join(passenger.ref, 1, Up)
    }
    "時間経過とによってエレベータが移動する" in {
      // TODO test
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
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, Nil))(ElevatorProps(building.ref)))
      elevator ! Enter(passenger.ref, 5)
      building.expectMessage(Entered(passenger.ref).callback)
      building.expectMessage(StopAdded(5 :: Nil).callback)
    }
    "行き先未指定の場合は動かない" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, Nil))(ElevatorProps(building.ref)))
      elevator ! Execute
      building.expectMessage(StateKept(Stopping).callback)
    }
    "行き先の方向に動き出す" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(1, 2 :: Nil))(ElevatorProps(building.ref)))
      elevator ! Execute
      building.expectMessage(StateChanged(Moving).callback)
    }
  }

  "動いているエレベーターは" should {
    "乗客が乗り込めない" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val passenger = testKit.createTestProbe[Passenger.PassengerMsg]()
      val elevator = testKit.spawn(Elevator.moving(ElevatorState(1, Nil))(Elevator.ElevatorProps(building.ref)))
      elevator ! Enter(passenger.ref, 5)
      building.expectMessage(EnterError(passenger.ref).callback)
    }
    "現状の移動方向に対する行き先の追加は途中であっても追加される" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.moving(ElevatorState(5, Nil))(ElevatorProps(building.ref)))
      elevator ! AddStop(10)
      building.expectMessage(StopAdded(10 :: Nil).callback)
    }
    "現状の移動方向に対する逆方向の行き先の追加は現在の移動の完了後に追加される" in {
      // TODO test
    }
    "現在の階は行き先に追加できない" in {
      // TODO test
    }
  }

  "行き先が空のエレベータには" should {
    "現在の階以外の行き先階を追加できる" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(5, Nil))(ElevatorProps(building.ref)))
      elevator ! AddStop(10)
      building.expectMessage(StopAdded(10 :: Nil).callback)
    }
    "現在の階は行き先に追加できない" in {
      // TODO test
    }
  }
  "行き先が空ではないエレベータは" should {
    "現在の行き先と逆方向への行き先の追加は後回しになる" in {
      // TODO moving/stoppingの両方テスト
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(5, 10 :: Nil))(ElevatorProps(building.ref)))
      elevator ! AddStop(1)
      building.expectMessage(StopAdded(10 :: 1 :: Nil).callback)
    }
    "現在の行き先方向の途中への行き先追加は途中に挿入される" in {
      val building = testKit.createTestProbe[ElevatorCallback]()
      val elevator = testKit.spawn(Elevator.stopping(ElevatorState(5, 6 :: 10 :: Nil))(ElevatorProps(building.ref)))
      elevator ! AddStop(7)
      building.expectMessage(StopAdded(6 :: 7 :: 10 :: Nil).callback)
    }
  }
}

class PassengerSpec extends WordSpec with BeforeAndAfterAll with Matchers {
  val testKit = ActorTestKit()
  override def afterAll(): Unit = testKit.shutdownTestKit()

  "エレベータを待っている乗客は" should {
    "行き先の方向が一致したエレベーターに乗る" in {
      val building = testKit.createTestProbe[PassengerCallback]()
      val elevator = testKit.createTestProbe[ElevatorMsg]()
      val passenger = testKit.spawn(Passenger(PassengerProps(building.ref, 5, 1), 1, 5))
      passenger ! Arrived(elevator.ref, 1, Some(Up))
      elevator.expectMessage(Enter(passenger, 5))
    }
    "行き先がないエレベーターにも乗る" in {
      val building = testKit.createTestProbe[PassengerCallback]()
      val elevator = testKit.createTestProbe[ElevatorMsg]()
      val passenger = testKit.spawn(Passenger(PassengerProps(building.ref, 5, 1), 1, 5))
      passenger ! Arrived(elevator.ref, 1, None)
      elevator.expectMessage(Enter(passenger, 5))
    }
    "行き先が逆方向のエレベーターには乗らない" in {
      val building = testKit.createTestProbe[PassengerCallback]()
      val elevator = testKit.createTestProbe[ElevatorMsg]()
      val passenger = testKit.spawn(Passenger(PassengerProps(building.ref, 5, 1), 4, 5))
      passenger ! Arrived(elevator.ref, 4, Some(Down))
      elevator.expectMessage(Ignore(passenger))
    }
  }
  "エレベーターに乗っている乗客は" should {
    "目的の階で降りる" in {
      // TODO test
    }
    "目的の階までは降りない" in {
      // TODO test
    }
  }
  "目的階に滞在中の乗客は" should {
    "指定時間経過まで留まる" in {
      val building = testKit.createTestProbe[PassengerCallback]()
      val passenger = testKit.spawn(Passenger.staying(PassengerState(5), 3)(PassengerProps(building.ref, 5, 1)))
      passenger ! TickToPassenger(8)
      building.expectMessage(Staying().callback)
    }
    "指定時間経過後に1階に戻ろうとする" in {
      val building = testKit.createTestProbe[PassengerCallback]()
      val passenger = testKit.spawn(Passenger.staying(PassengerState(5), 3)(PassengerProps(building.ref, 5, 1)))
      passenger ! TickToPassenger(9)
      building.expectMessage(Waiting().callback)
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
