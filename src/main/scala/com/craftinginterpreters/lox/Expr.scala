package com.craftinginterpreters.lox

sealed trait Expr

case class Assign(token: Token, expr: Expr) extends Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Literal(value: Value) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Variable(token: Token) extends Expr
