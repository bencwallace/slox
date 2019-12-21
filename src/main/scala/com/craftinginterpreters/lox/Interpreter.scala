package com.craftinginterpreters.lox

object Interpreter {

  def interpret(statements: Seq[Stmt]): Unit =
    try {
      for (statement <- statements) execute(statement)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }

  private def execute(statement: Stmt): Unit = statement match {
    case Expression(expr) => {
      eval(expr)  // todo: why? just to check for error?
      ()
    }
    case Print(expr) => println(eval(expr).toString)
  }

  // todo: can this be implemented using map?
  // todo: try to simplify pattern match now that `Value` being used
  // note: very good use of pattern matching rather than visitor pattern + switch statement
  private def eval(expr: Expr): Value = expr match {
    case Literal(someValue) => someValue
    case Grouping(e) => eval(e)
    case Unary(t @ Token(MINUS, _, _, _), right) => eval(right) match {
      case Number(x) => Number(-x)
      case _ => throw new RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG, _, _, _), right) => eval(right) match {
      case Nil() | Bool(false) => Bool(false)
      case _ => Bool(true)
    }
    case Binary(left, Token(EQUAL_EQUAL, _, _, _), right) =>
      Bool(eval(left) == eval(right))
    case Binary(left, Token(BANG_EQUAL, _, _, _), right) =>
      Bool(eval(left) != eval(right))
    case Binary(left, token @ Token(t, _, _, _), right) =>
      (eval(left), eval(right)) match {
      case (Number(x), Number(y)) => t match {
        case MINUS => Number(x - y)
        case SLASH => Number(x / y)
        case STAR => Number(x * y)
        case PLUS => Number(x + y)
        case GREATER => Bool(x > y)
        case GREATER_EQUAL => Bool(x >= y)
        case LESS => Bool(x < y)
        case LESS_EQUAL => Bool(x <= y)
      }
      case (Str(x), Str(y)) => t match {
        case PLUS => Str(x + y)
      }
      case _ => throw new RuntimeError(token, "Operands must be numbers.")
    }
  }

}
