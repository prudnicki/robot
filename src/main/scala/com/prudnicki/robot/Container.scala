package com.prudnicki.robot

case class Container private (name: String,
                              capacity: Int,
                              filledAmount: Int) {

  def fill(amount: Int): Container = {
    require(amount >= 0, "Amount to fill must be non-negative")
    if (filledAmount + amount > capacity)
      throw ContainerFullException(name, amount)
    Container(name, capacity, filledAmount + amount)
  }

}

object Container {

  def apply(name: String, capacity: Int, filledAmount: Int = 0): Container = {
    require(name != null && name.nonEmpty,
            "Container's name must be non-empty String")
    require(capacity >= 0, "Container's capacity must be non-negative")
    require(filledAmount <= capacity,
            "Container's filled amount can't be greater than it's capacity")
    new Container(name, capacity, filledAmount)
  }

}

case class ContainerFullException(name: String, amount: Int) extends Exception {

  override def toString: String =
    s"Container $name cannot be filled with $amount because it would overflow"

}
