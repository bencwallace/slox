package com.craftinginterpreters.lox

class LoxClass(name: String) extends LoxCallable {
  override def toString: String = name

  override def arity: Int = ???

  override def call(interpreter: Interpreter, args: Seq[Value]): Value = ???
}
