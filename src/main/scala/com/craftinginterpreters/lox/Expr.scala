package com.craftinginterpreters.lox

sealed trait Expr {
  def parenthesize(name: String, exprs: Expr*): String = {
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

case class Assign(token: Token, expr: Expr) extends Expr {
  override def toString: String = token.literal match {
    case Some(value) => parenthesize("=", Literal(value), expr)
  }
}
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
  override def toString: String = parenthesize(operator.lexeme, left, right)
}
case class Call(callee: Expr, paren: Token, args: Seq[Expr]) extends Expr
case class Grouping(expr: Expr) extends Expr {
  override def toString: String = parenthesize("group", expr)
}
case class Literal(value: Value) extends Expr {
  override def toString: String = value match {
    case Nil => "nil"
    case _ => value.toString
  }
}
case class Unary(operator: Token, right: Expr) extends Expr {
  override def toString: String = parenthesize(operator.lexeme, right)
}
case class Variable(token: Token) extends Expr {
  override def toString: String = parenthesize("var", Literal(Str(token.lexeme)))
}
