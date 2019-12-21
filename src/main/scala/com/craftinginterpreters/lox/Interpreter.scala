package com.craftinginterpreters.lox

object Interpreter {

  def interpret(statements: Seq[Option[Stmt]]): Unit =
    try {
      // todo: understand how following line works
      //  and check whether it is intended behaviour
      for (Some(statement) <- statements) execute(statement)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }

  private def execute(statement: Stmt): Unit = statement match {
    case Expression(expr) => {
      eval(expr)
      ()
    }
    case Print(expr) => println(eval(expr).toString)
    case Var(_, _) => ???
  }

  private def eval(expr: Expr): Value = expr match {
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
        case _ => throw new RuntimeError(token, "Operands must be numbers.")
      }
    case Unary(t @ Token(MINUS, _, _, _), right) => eval(right) match {
      case Number(x) => Number(-x)
      case _ => throw new RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG, _, _, _), right) => eval(right) match {
      case Nil | Bool(false) => Bool(false)
      case _ => Bool(true)
    }
    case Unary(_, _) => ???
    case Grouping(e) => eval(e)
    case Literal(someValue) => someValue
    case Variable(_) => ???
  }

}
