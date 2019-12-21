package com.craftinginterpreters.lox

// double, boolean, nil, string
sealed trait Value

case class Bool(value: Boolean) extends Value {
  override def toString: String = value.toString
}
case class Nil() extends Value {
  override def toString: String = "nil"
}
case class Number(value: Double) extends Value {
  override def toString: String = value.toString
}
case class Str(value: String) extends Value {
  override def toString: String = value
}
