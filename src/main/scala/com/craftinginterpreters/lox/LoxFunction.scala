package com.craftinginterpreters.lox

class LoxFunction(declaration: Function, closure: Environment) extends LoxCallable {
  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, args: Seq[Value]): Value = {
    val environment = new Environment(Some(closure))

    for ((param, i) <- declaration.params.zipWithIndex)
      environment.define(param.lexeme, args(i))
    try {
      new Interpreter(environment).executeBlock(declaration.body)
    } catch {
      case ReturnException(value) => return value
    }
    NilVal
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
}