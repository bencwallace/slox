package com.craftinginterpreters.lox

import scala.collection.mutable

class Resolver(interpreter: Interpreter) {

  private sealed trait FunctionType
  private case object NONE extends FunctionType
  private case object FUNCTION extends FunctionType

  private val scopes = mutable.Stack[mutable.Map[String, Boolean]]()
  private var currentFunction: FunctionType = NONE

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
      resolveFunction(f, FUNCTION)
    // simple cases
    case Block(statements) =>
      beginScope()
      resolve(statements)
      endScope()
    case Class(name, _) =>
      declare(name)
      define(name)
    case Expression(expr) => resolve(expr)
    case If(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      elseBranch match {
        case None => ()
        case Some(branch) => resolve(branch)
      }
    case Print(expr) => resolve(expr)
    case Return(keyword, expr) => {
      currentFunction match {
        case NONE => Lox.error(keyword, "Cannot return from top-level code.")
        case _ => ()
      }
      resolve(expr)
    }
    case While(condition, body) =>
      resolve(condition)
      resolve(body)
  }

  private def resolve(expr: Expr): Unit = expr match {
    case Variable(name) =>
      if (scopes.nonEmpty) scopes.top.get(name.lexeme) match {
        case Some(false) => Lox.error(name, "Cannot read local variable in its own initializer.")
        case _ => ()
      }
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
//        interpreter.resolve(expr, scopes.size - 1 - i)
        interpreter.resolve(expr, i)
        return
      }

  private def resolveFunction(function: Function, functionType: FunctionType): Unit = {
    val enclosingFunction = currentFunction
    currentFunction = functionType
    beginScope()
    for (param <- function.params) {
      declare(param)
      define(param)
    }
    resolve(function.body)
    endScope()
    currentFunction = enclosingFunction
  }

  private def declare(name: Token): Unit =
    if (scopes.isEmpty) ()
    else if (scopes.top.contains(name.lexeme))
      Lox.error(name, "Variable with this name already declared in this scope.")
    else
      scopes.top += (name.lexeme -> false)

  private def define(name: Token): Unit =
    if (scopes.isEmpty) ()
    else scopes.top += (name.lexeme -> true)

  private def beginScope(): Unit = {
    scopes.push(mutable.Map())
    ()
  }

  private def endScope(): Unit = {
    scopes.pop()
    ()
  }

}
