package com.craftinginterpreters.lox

sealed trait Stmt

case class Block(statements: Seq[Stmt]) extends Stmt
case class Expression(expr: Expr) extends Stmt
case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt
case class Function(name: Token, params: Seq[Token], body: Seq[Stmt]) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Return(keyword: Token, expr: Expr) extends Stmt
case class Var(name: Token, init: Option[Expr]) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt

case object End extends Stmt
