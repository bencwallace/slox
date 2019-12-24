package com.craftinginterpreters.lox

object Token {
  def apply(tokenType: TokenType, lexeme: String, literal: Option[Value], line: Int): Token =
    new Token(tokenType, lexeme, literal, line)

  def unapply(token: Token): Option[(TokenType, Option[Value])] =
    Some(token.tokenType, token.literal)
}

// todo: use apply/unapply to match `tokenType` and `literal`
class Token(val tokenType: TokenType,
            val lexeme: String,
            val literal: Option[Value],
            val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme + " " + literal
}
