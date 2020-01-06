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

class LoxFunction(declaration: Function, closure: Environment) extends LoxCallable {
  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, args: Seq[Value]): Value = {
    val environment = new Environment(Some(closure))

    for ((param, i) <- declaration.params.zipWithIndex)
      environment.define(param.lexeme, args(i))
    try {
      //      new Interpreter(environment).executeBlock(declaration.body)
      interpreter.executeBlock(declaration.body, environment)
    } catch {
      case ReturnException(value) => return value
    }
    NilVal
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"

  def bind(instance: LoxInstance): LoxFunction = {
    val environment = new Environment(Some(closure))
    environment.define("this", instance)
    new LoxFunction(declaration, environment)
  }
}

class LoxClass(name: String, methods: Map[String, LoxFunction]) extends LoxCallable {
  override def toString: String = name

  override def arity: Int = 0

  override def call(interpreter: Interpreter, args: Seq[Value]): Value =
    new LoxInstance(this)

  def findMethod(name: String): Option[LoxFunction] = methods.get(name)
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
