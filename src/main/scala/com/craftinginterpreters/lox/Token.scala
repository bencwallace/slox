package com.craftinginterpreters.lox

case class Token(tokenType: TokenType,
                 lexeme: String,
                 literal: Option[Value],
                 line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme + " " + literal
}
