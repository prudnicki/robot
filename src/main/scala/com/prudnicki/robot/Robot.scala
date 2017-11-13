package com.prudnicki.robot

import scala.util.Try

class Robot(initialFloor: Floor) {

  // I've decided to memorize whole floor states as history
  // this will be more memory-consuming than executing moves and their reverses each time undo/redo is called
  // but will be much faster in terms of computational complexity
  // Also, this means that I assume there is only one Robot operating given floor at given time (so no concurrency either)
  private var history: Seq[Floor] = Vector[Floor](initialFloor)
  private var currentHistoryPosition: Int = 0

  def fill(containerName: String, amount: Int): Try[Floor] = Try {
    val stackIndex = findIndexOfStackWithContainer(containerName)
    if (stackAt(stackIndex).topContainer.name != containerName)
      throw ContainerNotOnTopException(containerName)
    val stackBefore = stackAt(stackIndex)
    val stackAfter =
      stackBefore.withTopReplaced(stackBefore.topContainer.fill(amount))
    val nextFloorState =
      currentFloorState.withStackReplaced(stackIndex, stackAfter)
    appendToHistory(nextFloorState)
    nextFloorState
  }

  private def findIndexOfStackWithContainer(containerName: String): Int = {
    val index =
      currentFloorState.stacks.indexWhere(s => s.hasContainer(containerName))
    if (index == -1)
      throw ContainerNotFoundException(containerName)
    index
  }

  private def stackAt(stackIndex: Int) = {
    currentFloorState
      .stacks(stackIndex)
  }

  private def currentFloorState: Floor = history(currentHistoryPosition)

  private def appendToHistory(floor: Floor): Unit = {
    currentHistoryPosition = currentHistoryPosition + 1
    //once we execute any action future states of history are no longer valid and redo operation becomes unavailable
    val newHistory = history.take(currentHistoryPosition) :+ floor
    history = newHistory
  }

  def move(movedContainerName: String,
           targetContainerName: String): Try[Floor] = Try {
    val movedStackIndex =
      findIndexOfStackWithContainer(movedContainerName)
    val targetStackIndex =
      findIndexOfStackWithContainer(targetContainerName)
    val nextFloorState =
      if (movedStackIndex != targetStackIndex)
        moveAcrossSeparateStacks(movedContainerName,
                                 targetContainerName,
                                 movedStackIndex,
                                 targetStackIndex)
      else
        moveInTheSameStack(movedContainerName,
                           targetContainerName,
                           movedStackIndex)
    appendToHistory(nextFloorState)
    nextFloorState
  }

  private def moveAcrossSeparateStacks(movedContainerName: String,
                                       targetContainerName: String,
                                       movedStackIndex: Int,
                                       targetStackIndex: Int): Floor = {
    val targetContainerStack = stackAt(targetStackIndex)

    //according to task description we shouldn't ever move 'target' container
    if (targetContainerStack.topContainer.name != targetContainerName)
      throw CantMoveOnTopOfOccupiedContainerException(movedContainerName,
                                                      targetContainerName)

    if (targetContainerStack.containers.length >= currentFloorState.maxHeight)
      throw TargetStackFullException(movedContainerName, targetContainerName)

    var tempFloorState = currentFloorState
    while (tempFloorState
             .stacks(movedStackIndex)
             .topContainer
             .name != movedContainerName) {
      tempFloorState =
        moveTopContainer(tempFloorState, movedStackIndex, targetStackIndex)
    }

    val (movedContainer, movedContainerStackAfterMove) =
      tempFloorState.stacks(movedStackIndex).pop
    val targetContainerStackAfterMove = ContainerStack(
      movedContainer +: tempFloorState.stacks(targetStackIndex).containers)
    val newFloorState = tempFloorState
      .withStackReplaced(movedStackIndex, movedContainerStackAfterMove)
      .withStackReplaced(targetStackIndex, targetContainerStackAfterMove)
    newFloorState
  }

  private def moveTopContainer(floor: Floor,
                               movedStackIndex: Int,
                               targetStackIndex: Int): Floor = {
    val freeStackIndex = floor.stacks.zipWithIndex
      .indexWhere {
        case (stack, index) =>
          index != movedStackIndex && index != targetStackIndex && stack.containers.length < floor.maxHeight
      }

    if (freeStackIndex == -1)
      throw CannotFindStackForContainersOnTopOfMovedException()

    val (poppedContainer, newMovedStack) = floor.stacks(movedStackIndex).pop
    val newFloorState = floor
      .withStackReplaced(
        freeStackIndex,
        floor.stacks(freeStackIndex).withContainerOnTop(poppedContainer))
      .withStackReplaced(movedStackIndex, newMovedStack)
    newFloorState
  }

  private def moveInTheSameStack(movedContainerName: String,
                                 targetContainerName: String,
                                 stackIndex: Int): Floor = {
    // "A container can be moved on top of a destination container
    // only if there isn't already another container on top of the destination"
    // So we check whether containers are already ordered correctly
    val alreadyOnTop = stackAt(stackIndex)
      .isContainerOnTopOfAnother(movedContainerName, targetContainerName)
    if (!alreadyOnTop)
      throw CantMoveOnTopOfOccupiedContainerException(movedContainerName,
                                                      targetContainerName)
    currentFloorState
  }

  def undo(): Floor = {
    if (currentHistoryPosition == 0) currentFloorState
    else {
      currentHistoryPosition = currentHistoryPosition - 1
      currentFloorState
    }
  }

  def redo(): Floor = {
    if (currentHistoryPosition == history.length - 1) currentFloorState
    else {
      currentHistoryPosition = currentHistoryPosition + 1
      currentFloorState
    }
  }

}

case class ContainerNotFoundException(name: String) extends Exception {

  override def toString: String = s"Container named $name couldn't be found"

}

case class ContainerNotOnTopException(name: String) extends Exception {

  override def toString: String =
    s"Container $name cannot be filled because it is not on the top of it's stack"

}

case class CantMoveOnTopOfOccupiedContainerException(movedName: String,
                                                     targetName: String)
    extends Exception {

  override def toString: String =
    s"Container $movedName cannot be moved on top of $targetName, as $targetName is not on top of it's stack"

}

case class TargetStackFullException(movedName: String, targetName: String)
    extends Exception {

  override def toString: String =
    s"Container $movedName cannot be moved on top of $targetName, as $targetName's is of max height"

}

case class CannotFindStackForContainersOnTopOfMovedException()
    extends Exception {

  override def toString: String =
    s"Cannot find a free/not full stack for a container to move to"

}
