package com.craftinginterpreters.lox

class LoxClass(name: String, methods: Map[String, LoxFunction]) extends LoxCallable {
  override def toString: String = name

  override def arity: Int = 0

  override def call(interpreter: Interpreter, args: Seq[Value]): Value =
    new LoxInstance(this)

  def findMethod(name: String): Option[LoxFunction] = methods.get(name)
}
