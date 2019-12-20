package com.craftinginterpreters.lox

class Parser(tokens: Seq[Token]) {

  var current = 0

  private case class ParseError() extends RuntimeException

  def parse(): Option[Expr] =
    try {
      Some(expression())
    } catch {
      case ParseError() => None
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

  private def expression(): Expr = equality()

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
    if (matchTokens(FALSE)) Literal(Some(false))
    else if (matchTokens(TRUE)) Literal(Some(true))
    else if (matchTokens(NIL)) Literal(None)
    else if (matchTokens(NUMBER, STRING)) Literal(previous.literal)
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

  private def check(tokenType: TokenType): Boolean = if (isAtEnd) false else peek.tokenType == tokenType

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

  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      if (previous.tokenType == SEMICOLON) ()
      else peek.tokenType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => ()
        case _ => advance()
      }
    }
  }

}
