package com.craftinginterpreters.lox

// double, boolean, nil, string
sealed trait Value {
  def isTruthy: Boolean = this match {
    case Bool(value) => value
    case NilVal => false
    case _ => true
  }

  override def toString: String = this match {
    case LoxCallable() => "LoxCallable"
    case Bool(value) => value.toString
    case NilVal => "nil"
    case Number(value) =>
      val text = value.toString
      if (text.endsWith(".0")) text.substring(0, text.length - 2)
      else text
    case Str(value) => value
  }
}

abstract case class LoxCallable() extends Value {
  def arity: Int
  def call(interpreter: Interpreter, args: Seq[Value]): Value
}

case class Bool(value: Boolean) extends Value
case object NilVal extends Value
case class Number(value: Double) extends Value
case class Str(value: String) extends Value
