package com.craftinginterpreters.lox

import scala.collection.mutable

object Interpreter {

  val globals = new Environment()
  globals.define("clock", new LoxCallable {
    override def arity: Int = 0

    override def call(interpreter: Interpreter, args: Seq[Value]): Value =
      Number(System.currentTimeMillis / 1000.0)
  })

}

class Interpreter() {

  private var environment = Interpreter.globals
  private val locals = mutable.Map[Expr, Int]()

  def interpret(statements: Seq[Stmt]): Unit =
    try {
      for (statement <- statements) execute(statement)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error)
    }

  private def execute(statement: Stmt): Unit = statement match {
    case Block(statements) =>
      executeBlock(statements, new Environment(Some(environment)))
    case Class(name, superclass, methods) =>
      val sClass = superclass map {
        case s @ Variable(n) => eval(s) match {
          case lc @ LoxClass() => lc
          case _ => throw RuntimeError(n, "Superclass must be a class.")
        }
      }
      environment.define(name.lexeme, NilVal)
      sClass match {
        case None => ()
        case Some(s) =>
          environment = new Environment(Some(environment))
          environment.define("super", s)
      }
      val ms =
        for (method <- methods)
          yield method.name.lexeme -> new LoxFunction(method, environment, method.name.lexeme.equals("init"))
      val klass = new LoxClass(name.lexeme, sClass, ms.toMap)
      (superclass, environment.enclosing) match {
        case (None, _) => ()
        case (Some(_), Some(e)) => environment = e
      }
      environment.assign(name, klass)
    case Expression(expr) =>
      eval(expr)
      ()
    case If(condition, thenBranch, elseBranch) =>
      if (eval(condition).isTruthy) execute(thenBranch)
      else elseBranch match {
        case Some(s) => execute(s)
        case None => ()
      }
    case f @ Function(name, _, _) =>
      environment.define(name.lexeme, new LoxFunction(f, environment))
    case Print(expr) => println(eval(expr))
    case Return(_, expr) => throw ReturnException(eval(expr))
    case Var(name, None) => environment.define(name.lexeme, NilVal)
    case Var(name, Some(expr)) => environment.define(name.lexeme, eval(expr))
    case While(condition, body) => while(eval(condition).isTruthy) execute(body)
    case End => ???
  }

  private[lox] def resolve(expr: Expr, depth: Int): Unit =
    locals += (expr -> depth)


  private[lox] def executeBlock(statements: Seq[Stmt], environment: Environment): Unit = {
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
    case Assign(token, right) =>
      val value = eval(right)
      locals.get(expr) match {
        case None => Interpreter.globals.assign(token, value)
        case Some(d) => environment.assignAt(d, token, value)
      }
      value
    case Binary(left, Token(AND), right) =>
      val l = eval(left)
      if (!l.isTruthy) l else eval(right)
    case Binary(left, Token(OR), right) =>
      val l = eval(left)
      if (l.isTruthy) l else eval(right)
    case Binary(left, token @ Token(t), right) =>
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
    case Call(callee, paren, args) =>
      val vals = for { arg <- args } yield eval(arg)
      val fcn = eval(callee)
      fcn match {
        case f @ LoxCallable() =>
          if (vals.size != f.arity)
            throw RuntimeError(paren, s"Expected ${f.arity} arguments but got ${args.size}.")
          else f. call(this, vals)
        case _ => throw RuntimeError(paren, "Can only call functions and classes.")
      }
    case Get(obj, name) =>
      eval(obj) match {
        case objVal @ LoxInstance(_) => objVal.get(name)
        case _ => throw new RuntimeError(name, "Only instances have properties.")
      }
    case Grouping(e) => eval(e)
    case Literal(someValue) => someValue
    case Set(obj, name, value) => eval(obj) match {
      case objVal @ LoxInstance(_) =>
        val v = eval(value)
        objVal.set(name, v)
        v
      case _ => throw new RuntimeError(name, "Only instances have fields.")
    }
    case Super(_, method) => {
      val (superclass, obj) = locals.get(expr) match {
        case None => ???
        case Some(d) => (environment.getAt(d, "super"), environment.getAt(d - 1, "this"))
      }
      val m = superclass match {
        case (lc @ LoxClass()) => lc.findMethod(method.lexeme)
        case _ => ???
      }
      (m, obj) match {
        case (None, _) => throw RuntimeError(method, s"Undefined property '${method.lexeme}'.'")
        case (Some(loxFunction), instance @ LoxInstance(_)) => loxFunction.bind(instance)
      }
    }
    case This(keyword) => lookUpVariable(keyword, expr)
    case Unary(t @ Token(MINUS), right) => eval(right) match {
      case Number(x) => Number(-x)
      case _ => throw RuntimeError(t, "Operand must be a number.")
    }
    case Unary(Token(BANG), right) => Bool(!eval(right).isTruthy)
    case Variable(token) => lookUpVariable(token, expr)
    case _ => ???
  }

  private def lookUpVariable(token: Token, expr: Expr): Value = locals.get(expr) match {
    case None => Interpreter.globals.get(token)
    case Some(d) => environment.getAt(d, token.lexeme)
  }

}
