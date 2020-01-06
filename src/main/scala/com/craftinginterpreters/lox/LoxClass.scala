package com.craftinginterpreters.lox

class LoxClass(name: String) extends LoxCallable {
  override def toString: String = name

  override def arity: Int = 0

  override def call(interpreter: Interpreter, args: Seq[Value]): Value =
    new LoxInstance(this)
}
