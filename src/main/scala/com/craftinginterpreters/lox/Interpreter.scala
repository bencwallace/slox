package com.craftinginterpreters.lox

object Interpreter {

  def interpret(expr: Expr): Unit = {
    try {
      val value = eval(expr)
      println(stringify(value))
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }
  }

  // note: very good use of pattern matching rather than visitor pattern + switch statement
  private def eval(expr: Expr): Option[Any] = expr match {
    case Literal(someValue) => someValue
    case Grouping(e) => eval(e)
    case Unary(t @ Token(MINUS, _, _, _), right) => eval(right) match {
      case Some(r: Double) => Some(-r)
      case _ => throw new RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG, _, _, _), right) => eval(right) match {
      case None | Some(false) => None
      case _ => Some(true)
    }
    case Binary(left, Token(EQUAL_EQUAL, _, _, _), right) => Some(eval(left) == eval(right))
    case Binary(left, Token(BANG_EQUAL, _, _, _), right) => Some(eval(left) != eval(right))
    case Binary(left, token @ Token(t, _, _, _), right) => (eval(left), eval(right)) match {
      case (Some(x: Double), Some(y: Double)) => t match {
        case MINUS => Some(x - y)
        case SLASH => Some(x / y)
        case STAR => Some(x * y)
        case PLUS => Some(x + y)
        case GREATER => Some(x > y)
        case GREATER_EQUAL => Some(x >= y)
        case LESS => Some(x < y)
        case LESS_EQUAL => Some (x <= y)
        case EQUAL_EQUAL => None
      }
      case (Some(x: String), Some(y: String)) => t match {
        case PLUS => Some(x + y)
      }
      case _ => throw new RuntimeError(token, "Operands must be numbers.")
    }
  }

  private def stringify(option: Option[Any]): String = option match {
    case None => "nil"
    case Some(x: Double) => {
      val s = x.toString
      if (s.endsWith(".0")) s.substring(0, s.length - 2)
      else s
    }
    case Some(x) => x.toString
  }

}
