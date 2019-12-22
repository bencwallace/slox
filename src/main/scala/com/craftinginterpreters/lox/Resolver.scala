package com.craftinginterpreters.lox

import scala.collection.mutable.{Map,Stack}

class Resolver(interpreter: Interpreter) {

  private val scopes = Stack[Map[String, Boolean]]()

  def resolve(statements: Seq[Stmt]): Unit =
    for (statement <- statements)
      resolve(statement)

  private def resolve(statement: Stmt): Unit = statement match {
    case Var(name, None) => {
      declare(name)
      define(name)
    }
    case Var(name, Some(init)) => {
      declare(name)
      resolve(init)
      define(name)
    }
    case f @ Function(name, params, body) => {
      declare(name)
      define(name)
      resolveFunction(f)
    }
  }

  private def resolve(expr: Expr): Unit = expr match {
    case Variable(name) => {
      if (!scopes.isEmpty && scopes.top(name.lexeme) == false)
        Lox.error(name, "Cannot read local variable in its own initializer.")
      resolveLocal(expr, name)
    }
    case Assign(name, expr) => {
      resolve(expr)
      resolveLocal(expr, name)
    }
  }

  private def resolveLocal(expr: Expr, name: Token): Unit =
    for ((scope, i) <- scopes.zipWithIndex)
      if (scope.contains(name.lexeme)) {
        interpreter.resolve(expr, scopes.size - 1 - i)
        return
      }

  private def resolveFunction(function: Function): Unit = {
    beginScope()
    for (param <- function.params) {
      declare(param)
      define(param)
    }
    resolve(function.body)
    endScope()
  }

  // todo: resolving other syntax tree nodes

  private def declare(name: Token): Unit =
    if (scopes.isEmpty) return
    else scopes.top += (name.lexeme -> false)

  private def define(name: Token): Unit =
    if (scopes.isEmpty) return
    else scopes.top += (name.lexeme -> true)

  private def beginScope(): Unit = scopes.push(Map())

  private def endScope(): Unit = scopes.pop()

}
