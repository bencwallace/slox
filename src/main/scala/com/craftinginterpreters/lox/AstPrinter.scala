package com.craftinginterpreters.lox

object AstPrinter {

  // note: pattern matching used instead of visitor pattern
  def print(expr: Expr): String = expr match {
    case Binary(left, operator, right) => parenthesize(operator.lexeme, left, right)
    case Grouping(expr) => parenthesize("group", expr)
    case Unary(operator, right) => parenthesize(operator.lexeme, right)
    case Literal(None) => "nil"
    case Literal(Some(value)) => value.toString
  }

  def parenthesize(name: String, exprs: Expr*): String = {
    val builder = new StringBuilder()

    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ")
      builder.append(print(expr))
    }
    builder.append(")")

    builder.toString
  }

  def main(args: Array[String]): Unit = {
    val expression = new Binary(
      new Unary(
        Token(MINUS, "-", None, 1),
        new Literal(Some(123))
      ),
      Token(STAR, "*", None, 1),
      new Grouping(
        new Literal(Some(45.67))
      )
    )

    println(print(expression))
  }

}
