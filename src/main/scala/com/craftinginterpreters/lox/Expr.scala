package com.craftinginterpreters.lox

sealed trait Expr {
  override def toString: String = this match {
    case Assign(t @ Token(NUMBER), expr) => parenthesize("=", Literal(Number(t.lexeme.toDouble)), expr)
    case Assign(t @ Token(STRING), expr) => parenthesize("=", Literal(Str(t.lexeme)), expr)
    case Binary(left, op, right) => parenthesize(op.lexeme, left, right)
    case Call(callee, _, args) => parenthesize(callee.toString, args:_*)
    case Grouping(expr) => expr.toString
    case Literal(NilVal) => "nil"
    case Literal(value) => value.toString
    case Unary(op, right) => parenthesize(op.lexeme, right)
    case Variable(token) => parenthesize("var", Literal(Str(token.lexeme)))
    case _ => ???
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    val builder = new StringBuilder()

    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ")
      builder.append(expr.toString)
    }
    builder.append(")")

    builder.toString
  }
}

// todo: implement anonymous functions

case class Assign(token: Token, expr: Expr) extends Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Call(callee: Expr, paren: Token, args: Seq[Expr]) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Literal(value: Value) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Variable(token: Token) extends Expr