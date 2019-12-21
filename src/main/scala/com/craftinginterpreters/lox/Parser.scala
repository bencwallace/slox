package com.craftinginterpreters.lox

import scala.collection.mutable.ArrayBuffer

class Parser(tokens: Seq[Token]) {

  var current = 0

  private case class ParseError() extends RuntimeException

  def parse(): Seq[Option[Stmt]] = {
    val statements = ArrayBuffer[Option[Stmt]]()
    while (!isAtEnd)
      statements += declaration()
    statements.toSeq
  }

  // statement parsers

  private def declaration(): Option[Stmt] =
    try {
      if (matchTokens(VAR)) Some(varDeclaration())
      else Some(statement())
    } catch {
      case error: ParseError => {
        synchronize()
        None
      }
    }

  private def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect variable name.")
    val init = if (matchTokens(EQUAL)) Some(expression()) else None
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, init)
  }

  private def statement(): Stmt =
    if (matchTokens(PRINT)) printStatement() else expressionStatement()

  private def printStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(expr)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  // expression parsers

  // note: nice application of higher-order functions
  private def binary(tokenTypes: TokenType*)(nextParser: () => Expr): Expr = {
    var expr = nextParser()
    while (matchTokens(tokenTypes:_*)) {
      val operator = previous
      val right = nextParser()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def expression(): Expr = assignment()

  private def assignment(): Expr = {
    val expr = equality()

    if (matchTokens(EQUAL)) {
      val equals = previous
      val expr = assignment()

      expr match {
        case Variable(token) => Assign(token, expr)
        case _ => // todo: jlox doesn't throw error but not clear what else to do here
          throw error(equals, "Invalid assignment target.")
      }
    } else expr
  }

  private def equality(): Expr = binary(BANG_EQUAL, EQUAL_EQUAL)(comparison)

  private def comparison(): Expr = binary(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)(addition)

  private def addition(): Expr = binary(MINUS, PLUS)(multiplication)

  private def multiplication(): Expr = binary(SLASH, STAR)(unary)

  private def unary(): Expr =
    if (matchTokens(BANG, MINUS)) {
      val operator = previous
      val right = unary()
      Unary(operator, right)
    } else primary()


  private def primary(): Expr =
    if (matchTokens(FALSE)) Literal(Bool(false))
    else if (matchTokens(TRUE)) Literal(Bool(true))
    else if (matchTokens(NIL)) Literal(Nil)
    else if (matchTokens(NUMBER, STRING)) previous.literal match {
      case Some(value) => Literal(value)
      case None => ???
    }
    else if (matchTokens(IDENTIFIER)) Variable(previous)
    else if (matchTokens(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    } else throw error(peek, "Expect expression.")

  // utility methods

  private def matchTokens(tokenTypes: TokenType*): Boolean = {
    for (tokenType <- tokenTypes) {
      if (check(tokenType)) {
        advance()
        return true
      }
    }
    false
  }

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }

  private def check(tokenType: TokenType): Boolean =
    if (isAtEnd) false else peek.tokenType == tokenType

  private def consume(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) advance()
    else throw error(peek, message)

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    new ParseError()
  }

  private def isAtEnd: Boolean = peek.tokenType == EOF

  private def peek: Token = tokens(current)

  private def previous = tokens(current - 1)

  // todo: fix this
  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      if (previous.tokenType == SEMICOLON) return ()
      else peek.tokenType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return ()
        case _ => advance()
      }
    }
  }

}
