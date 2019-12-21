package com.craftinginterpreters.lox

sealed trait Stmt

case class Block(statements: Seq[Stmt]) extends Stmt
case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Var(name: Token, init: Option[Expr]) extends Stmt

case object End extends Stmt
