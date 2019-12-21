package com.craftinginterpreters.lox

case class Token(val tokenType: TokenType, val lexeme: String, val literal: Option[Value], val line: Int) {
  override def toString: String = tokenType.toString + " " + lexeme + " " + literal
}
