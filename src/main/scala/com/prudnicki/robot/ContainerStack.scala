package com.prudnicki.robot

case class ContainerStack(containers: List[Container] = Nil) {

  def hasContainer(containerName: String): Boolean =
    containers.exists(_.name == containerName)

  def pop: (Container, ContainerStack) = {
    if (containers.isEmpty) throw CantPopEmptyStackException()
    (containers.head, ContainerStack(containers.tail))
  }

  def topContainer: Container = containers.head

  def withTopReplaced(newTop: Container): ContainerStack = {
    if (containers.isEmpty) throw CantReplaceTopOfEmptyStackException()
    ContainerStack(newTop :: containers.tail)
  }

  def withContainerOnTop(newTop: Container): ContainerStack =
    ContainerStack(newTop :: containers)

  def isContainerOnTopOfAnother(containerOnTop: String,
                                containerBelowIt: String): Boolean = {
    val topContainerIndex = containers.indexWhere(c => c.name == containerOnTop)
    if (topContainerIndex == -1 || (topContainerIndex + 1) == containers.length)
      false
    else if (containers(topContainerIndex + 1).name == containerBelowIt) true
    else false
  }

}

case class CantPopEmptyStackException() extends Exception

case class CantReplaceTopOfEmptyStackException() extends Exception
