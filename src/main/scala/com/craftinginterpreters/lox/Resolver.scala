package com.craftinginterpreters.lox

import scala.collection.mutable.{Map,Stack}

class Resolver(interpreter: Interpreter) {

  private val scopes = Stack[Map[String, Boolean]]()

  def resolve(statements: Seq[Stmt]): Unit =
    for (statement <- statements)
      resolve(statement)

  private def resolve(statement: Stmt): Unit = statement match {
    case Var(name, initializer) =>
      declare(name)
      initializer match {
        case None => ()
        case Some(init) => resolve(init)
      }
      define(name)
    case f @ Function(name, _, _) =>
      declare(name)
      define(name)
      resolveFunction(f)
    // simple cases
    case Expression(expr) => resolve(expr)
    case If(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      elseBranch match {
        case None => ()
        case Some(branch) => resolve(branch)
      }
    case Print(expr) => resolve(expr)
    case Return(_, expr) => resolve(expr)
    case While(condition, body) =>
      resolve(condition)
      resolve(body)
  }

  private def resolve(expr: Expr): Unit = expr match {
    case Variable(name) =>
      if (!scopes.isEmpty && scopes.top(name.lexeme) == false)
        Lox.error(name, "Cannot read local variable in its own initializer.")
      resolveLocal(expr, name)
    case Assign(name, expr) =>
      resolve(expr)
      resolveLocal(expr, name)
    // simple cases
    case Binary(left, _, right) =>
      resolve(left)
      resolve(right)
    case Call(callee, _, args) =>
      resolve(callee)
      for (arg <- args)
        resolve(arg)
    case Grouping(expr) => resolve(expr)
    case Literal(_) => ()
    case Unary(_, right) => resolve(right)
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
