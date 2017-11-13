package com.prudnicki.robot

case class Floor private (stacks: Seq[ContainerStack],
                          maxHeight: Int,
                          maxStacks: Int) {

  def withStackReplaced(stackIndex: Int, newStack: ContainerStack): Floor = {
    val (beforeNewStack, afterNewStack) = stacks.splitAt(stackIndex)
    this.copy(stacks = beforeNewStack ++ Seq(newStack) ++ afterNewStack.tail)
  }

}

object Floor {

  def apply(stacks: Seq[ContainerStack],
            maxHeight: Int,
            maxStacks: Int): Floor = {
    require(maxHeight > 0, "Floor must allow stacks of at least 1 height")
    require(maxStacks > 0, "Floor must allow at least 1 container stack")
    val uniqueNames = stacks.flatMap(_.containers.map(c => c.name)).toSet
    require(uniqueNames.size == stacks.flatMap(_.containers).size,
            "Every container must have unique name")
    require(
      stacks.length <= maxStacks,
      "Starting stacks must be fewer than specified maximum number of stacks")
    new Floor(stacks ++ Seq.fill(maxStacks - stacks.size)(ContainerStack()),
              maxHeight,
              maxStacks)
  }

}
