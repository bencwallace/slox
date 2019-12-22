package com.craftinginterpreters.lox

object Interpreter {

  val globals = new Environment()
  globals.define("clock", new LoxCallable {
    override def arity: Int = 0

    override def call(interpreter: Interpreter, args: Seq[Value]): Value =
      Number(System.currentTimeMillis / 1000.0)
  })

}

class Interpreter(var environment: Environment = Interpreter.globals) {

  def interpret(statements: Seq[Stmt]): Unit =
    try {
      for (statement <- statements) execute(statement)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }

  private def execute(statement: Stmt): Unit = statement match {
    case Block(statements) =>
      (new Interpreter(new Environment(Some(environment)))).executeBlock(statements)
    case Expression(expr) => {
      eval(expr)
      ()
    }
    case If(condition, thenBranch, elseBranch) =>
      if (eval(condition).isTruthy) execute(thenBranch)
      else elseBranch match {
        case Some(s) => execute(s)
        case None => ()
      }
    case f @ Function(name, _, _) =>
      environment.define(name.lexeme, new LoxFunction(f, environment))
    case Print(expr) => println(eval(expr).toString)
    case Return(_, expr) => throw new ReturnException(eval(expr))
    case Var(name, None) => environment.define(name.lexeme, NilVal)
    case Var(name, Some(expr)) => environment.define(name.lexeme, eval(expr))
    case While(condition, body) => while(eval(condition).isTruthy) execute(body)
    case End => ???
  }

  private[lox] def executeBlock(statements: Seq[Stmt]): Unit =
    for (statement <- statements) execute(statement)

  private def eval(expr: Expr): Value = expr match {
    case Assign(token, expr) => {
      val value = eval(expr)
      environment.assign(token, value)
      value
    }
    case Binary(left, Token(AND, _, _, _), right) => {
      val l = eval(left)
      if (!l.isTruthy) l else eval(right)
    }
    case Binary(left, Token(OR, _, _, _), right) => {
      val l = eval(left)
      if (l.isTruthy) l else eval(right)
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
    case Call(callee, paren, args) => {
      val vals = for { arg <- args } yield eval(arg)
      val fcn = eval(callee)
      fcn match {
        case f @ LoxCallable() =>
          if (vals.size != f.arity)
            throw new RuntimeError(paren, s"Expected ${f.arity} arguments but got ${args.size}.")
          else f. call(this, vals)
        case _ => throw new RuntimeError(paren, "Can only call functions and classes.")
      }
    }
    case Grouping(e) => eval(e)
    case Literal(someValue) => someValue
    case Unary(t @ Token(MINUS, _, _, _), right) => eval(right) match {
      case Number(x) => Number(-x)
      case _ => throw RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG, _, _, _), right) => Bool(eval(right).isTruthy)
    case Variable(token) => environment.get(token)
    case _ => ???
  }

}
