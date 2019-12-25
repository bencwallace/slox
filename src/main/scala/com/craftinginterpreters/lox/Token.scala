package com.craftinginterpreters.lox

object Token {
  def unapply(token: Token): Option[TokenType] =
    Some(token.tokenType)
}

class Token(val tokenType: TokenType, val lexeme: String, val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme
}
