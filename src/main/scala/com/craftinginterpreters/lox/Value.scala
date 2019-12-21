package com.craftinginterpreters.lox

// double, boolean, nil, string
sealed trait Value {
  def isTruthy: Boolean = true
}

case class Bool(value: Boolean) extends Value {
  override def toString: String = value.toString
  override def isTruthy: Boolean = value
}
case object Nil extends Value {
  override def toString: String = "nil"
  override def isTruthy: Boolean = false
}
case class Number(value: Double) extends Value {
  override def toString: String = value.toString
}
case class Str(value: String) extends Value {
  override def toString: String = value
}
