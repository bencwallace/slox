// case classes much simplify this

package com.craftinginterpreters.lox

sealed trait Expr

case class Binary(val left: Expr, val operator: Token, val right: Expr) extends Expr
case class Unary(val operator: Token, val right: Expr) extends Expr
case class Grouping(val expr: Expr) extends Expr
case class Literal(val value: Option[Any]) extends Expr
