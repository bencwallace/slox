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
    case LoxInstance(klass) => s"<${klass.toString} instance>"
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
  def call(args: Seq[Value]): Value
}

class LoxFunction(declaration: Function,
                  closure: Environment,
                  isInitializer: Boolean = false) extends LoxCallable {
  override def arity: Int = declaration.params.size

  override def call(args: Seq[Value]): Value = {
    val environment = new Environment(Some(closure))

    for ((param, i) <- declaration.params.zipWithIndex)
      environment.define(param.lexeme, args(i))
    try {
      Interpreter.executeBlock(declaration.body, environment)
    } catch {
      case ReturnException(value) =>
        if (isInitializer) return closure.getAt(0, "this")
        else return value
    }
    if (isInitializer) closure.getAt(0, "this")
    else NilVal
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"

  def bind(instance: LoxInstance): LoxFunction = {
    val environment = new Environment(Some(closure))
    environment.define("this", instance)
    new LoxFunction(declaration, environment, isInitializer)
  }
}

object LoxClass {
  def unapply(loxClass: LoxClass): Boolean = true
}

class LoxClass(name: String, superclass: Option[LoxClass], methods: Map[String, LoxFunction]) extends LoxCallable {
  override def toString: String = name

  override def arity: Int = findMethod("init") match {
    case None => 0
    case Some(f) => f.arity
  }

  override def call(args: Seq[Value]): Value = {
    val instance = LoxInstance(this)
    findMethod("init") match {
      case Some(f) => f.bind(instance).call(args)
      case None => ()
    }
    instance
  }

  def findMethod(name: String): Option[LoxFunction] = (methods.get(name), superclass) match {
    case (Some(f), _) => Some(f)
    case (None, Some(sc)) => sc.findMethod(name)
    case _ => None
  }
}

case class LoxInstance(klass: LoxClass) extends Value {
  private val fields = mutable.Map[String, Value]()

  def get(name: Token): Value = fields.get(name.lexeme) match {
    case Some(v) => v
    case None => klass.findMethod(name.lexeme) match {
      case Some(m) => m.bind(this)
      case None =>
        throw RuntimeError(name, s"Undefined property '${name.lexeme}'.")
    }
  }

  def set(name: Token, value: Value): Unit = {
    fields += (name.lexeme -> value)
  }
}

case class Bool(value: Boolean) extends Value
case object NilVal extends Value
case class Number(value: Double) extends Value
case class Str(value: String) extends Value
