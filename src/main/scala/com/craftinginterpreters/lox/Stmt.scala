package com.craftinginterpreters.lox

sealed trait Stmt

case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
