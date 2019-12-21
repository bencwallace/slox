package com.craftinginterpreters.lox

class Interpreter {

  private var environment = new Environment()

  def interpret(statements: Seq[Stmt]): Unit =
    try {
      for (statement <- statements) execute(statement)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }

  private def execute(statement: Stmt): Unit = statement match {
    case Block(statements) => executeBlock(statements, new Environment(Some(environment)))
    case Expression(expr) => {
      eval(expr)
      ()
    }
    case Print(expr) => println(eval(expr).toString)
    case Var(name, None) => environment.define(name.lexeme, Nil)
    case Var(name, Some(expr)) => environment.define(name.lexeme, eval(expr))
    case End => ???
  }

  private def executeBlock(statements: Seq[Stmt], environment: Environment): Unit = {
    val previous = this.environment
    try {
      this.environment = environment
      for (statement <- statements)
        execute(statement)
    } finally {
      this.environment = previous
    }
  }

  private def eval(expr: Expr): Value = expr match {
    case Assign(token, expr) => {
      val value = eval(expr)
      environment.assign(token, value)
      value
    }
    case Binary(left, token @ Token(t, _, _, _), right) =>
      (eval(left), t, eval(right)) match {
        case (l, EQUAL_EQUAL, r) => Bool(l == r)
        case (l, BANG_EQUAL, r) => Bool(l != r)
        case (Str(x), PLUS, Str(y)) => Str(x + y)
        case (Number(x), PLUS, Number(y)) => Number(x + y)
        case (Number(x), MINUS, Number(y)) => Number(x - y)
        case (Number(x), SLASH, Number(y)) => Number(x / y)
        case (Number(x), STAR, Number(y)) => Number(x * y)
        case (Number(x), GREATER, Number(y)) => Bool(x > y)
        case (Number(x), GREATER_EQUAL, Number(y)) => Bool(x >= y)
        case (Number(x), LESS, Number(y)) => Bool(x < y)
        case (Number(x), LESS_EQUAL, Number(y)) => Bool(x <= y)
        case _ => throw RuntimeError(token, "Operands must be numbers.")
      }
    case Grouping(e) => eval(e)
    case Literal(someValue) => someValue
    case Unary(t @ Token(MINUS, _, _, _), right) => eval(right) match {
      case Number(x) => Number(-x)
      case _ => throw RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG, _, _, _), right) => eval(right) match {
      case Nil | Bool(false) => Bool(false)
      case _ => Bool(true)
    }
    case Variable(token) => environment.get(token)
    case _ => ???
  }

}
