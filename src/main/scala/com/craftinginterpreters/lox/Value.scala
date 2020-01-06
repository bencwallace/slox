package com.craftinginterpreters.lox

import scala.collection.mutable

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

case class LoxInstance(klass: LoxClass) extends Value {
  private val fields = mutable.Map[String, Value]()

  override def toString: String = s"${klass.toString} instance"

  def get(name: Token): Value = fields.get(name.lexeme) match {
    case Some(v) => v
    case None => klass.findMethod(name.lexeme) match {
      case Some(m) => m.bind(this)
      case None =>
        throw new RuntimeError(name, s"Undefined property '${name.lexeme}'.")
    }
  }

  def set(name: Token, value: Value): Unit = fields += (name.lexeme -> value)
}

case class Bool(value: Boolean) extends Value
case object NilVal extends Value
case class Number(value: Double) extends Value
case class Str(value: String) extends Value
