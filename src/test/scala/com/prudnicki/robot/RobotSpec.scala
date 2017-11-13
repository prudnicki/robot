package com.prudnicki.robot

import org.scalatest.{FlatSpecLike, Matchers}

class RobotSpec extends FlatSpecLike with Matchers {

  behavior of "Robot"

  private val defaultFloorSetup = floor(
    Seq(stack(cont("s1a"), cont("s1b")),
        stack(cont("s2a"), cont("s2b")),
        stack(cont("s3a"), cont("s3b"))))

  it should "fill containers properly" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.fill("s1a", 5)

    //then
    result.isSuccess shouldBe true
    val resultFloor = result.get
    resultFloor.stacks(0).topContainer.name shouldBe "s1a"
    resultFloor.stacks(0).topContainer.filledAmount shouldBe 5
  }

  it should "fail when trying to fill not-on-top container" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.fill("s1b", 5)

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[ContainerNotOnTopException]
  }

  it should "fail when trying to fill a container that is not on the floor" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.fill("s4a", 5)

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[ContainerNotFoundException]
  }

  it should "fail when trying to fill the container above it's max capacity" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.fill("s1a", 11)

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[ContainerFullException]
  }

  it should "move container which is on top of it's stack" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1a", "s2a")

    //then
    result.isSuccess shouldBe true
    val resultFloor = result.get
    resultFloor.stacks(0).topContainer.name shouldBe "s1b"
    resultFloor.stacks(1).topContainer.name shouldBe "s1a"
    resultFloor.stacks(1).containers(1).name shouldBe "s2a"
  }

  it should "move container which is NOT on top of it's stack" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1b", "s2a")

    //then
    result.isSuccess shouldBe true
    val resultFloor = result.get
    //top container from stack(0) should be moved on top of stack(2) to free second container in stack(0)
    resultFloor.stacks(0).containers shouldBe empty
    resultFloor.stacks(1).containers.length shouldBe 3
    resultFloor.stacks(1).topContainer.name shouldBe "s1b"
    resultFloor.stacks(1).containers(1).name shouldBe "s2a"
    resultFloor.stacks(2).containers.length shouldBe 3
    resultFloor.stacks(2).topContainer.name shouldBe "s1a"
  }

  it should "fail when it can't find a place for a container to move to" in {
    //given
    val fl = floor(
      Seq(stack(cont("s1a"), cont("s1b"), cont("s1c")),
          stack(cont("s2a"), cont("s2b")),
          stack(cont("s3a"), cont("s3b"))))
    val robot = new Robot(fl)

    //when
    val result = robot.move("s1c", "s2a")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[CannotFindStackForContainersOnTopOfMovedException]
  }

  it should "fail when asked to move a container on top of a container that's not on top of it's stack" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1a", "s2b")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[CantMoveOnTopOfOccupiedContainerException]
  }

  it should "fail when asked to move on top of full stack" in {
    //given
    val fl = floor(
      Seq(stack(cont("s1a"), cont("s1b"), cont("s1c")),
        stack(cont("s2a"), cont("s2b")),
        stack(cont("s3a"), cont("s3b"))))
    val robot = new Robot(fl)

    //when
    val result = robot.move("s2a", "s1a")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[TargetStackFullException]
  }

  it should "fail when asked to move a container that is not on the floor" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("not-existing", "s2a")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[ContainerNotFoundException]
  }

  it should "fail when asked to move on top of a container that is not on the floor" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1a", "not-existing")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[ContainerNotFoundException]
  }

  it should "report success when moving containers that are already on top of each other" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1a", "s1b")

    //then
    val resultFloor = result.get
    resultFloor.stacks(0).topContainer.name shouldBe "s1a"
    resultFloor.stacks(0).containers(1).name shouldBe "s1b"
  }

  it should "fail when trying to move containers in the same stack that are not on top of each other" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.move("s1b", "s1a")

    //then
    result.isSuccess shouldBe false
    val exception = result.failed.get
    exception shouldBe a[CantMoveOnTopOfOccupiedContainerException]
  }

  it should "undo operations successfully" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val resultAfterFirstMove = robot.move("s1a", "s2a")
    robot.move("s1b", "s3a")
    val result = robot.undo()

    //then
    result shouldBe resultAfterFirstMove.get
  }

  it should "return initial state when asked to undo when there were no actions performed" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    val result = robot.undo()

    //then
    result shouldBe defaultFloorSetup
  }

  it should "redo operations successfully" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    robot.move("s1a", "s2a")
    val resultAfterSecondMove = robot.move("s1b", "s3a")
    robot.undo()
    val result = robot.redo()

    //then
    result shouldBe resultAfterSecondMove.get
  }

  it should "return last floor state when asked to redo without undoing first" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    robot.move("s1a", "s2a")
    val resultAfterSecondMove = robot.move("s1b", "s3a")
    val result = robot.redo()

    //then
    result shouldBe resultAfterSecondMove.get
  }


  it should "return last floor state when asked to redo when another action was performed after undo" in {
    //given
    val robot = new Robot(defaultFloorSetup)

    //when
    robot.move("s1a", "s2a")
    robot.move("s1b", "s3a")
    robot.undo()
    robot.undo()
    val resultAfterThirdMove = robot.move("s1a", "s2a")
    val result = robot.redo()

    //then
    result shouldBe resultAfterThirdMove.get
  }


  def cont(name: String, capacity: Int = 10, filled: Int = 0): Container =
    Container(name, capacity, filled)

  def stack(containers: Container*): ContainerStack =
    ContainerStack(containers.toList)

  def floor(stacks: Seq[ContainerStack],
            maxHeight: Int = 3,
            maxStacks: Int = 3) = Floor(stacks, maxHeight, maxStacks)

}
