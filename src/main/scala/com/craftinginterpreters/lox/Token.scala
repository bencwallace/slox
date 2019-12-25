package com.craftinginterpreters.lox

object Token {
  def apply(tokenType: TokenType, lexeme: String, line: Int): Token =
    new Token(tokenType, lexeme, line)

  def unapply(token: Token): Option[(TokenType)] =
    Some(token.tokenType)
}

// todo: use apply/unapply to match `tokenType` and `literal`
class Token(val tokenType: TokenType,
            val lexeme: String,
//            val literal: Option[Value],
            val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme
}
