package com.craftinginterpreters.lox

class LoxFunction(declaration: Function) extends LoxCallable {
  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, args: Seq[Value]): Value = {
    val environment = new Environment(Some(Interpreter.globals))

    for ((param, i) <- declaration.params.zipWithIndex)
      environment.define(param.lexeme, args(i))
    new Interpreter(environment).executeBlock(declaration.body)
    NilVal
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
}
