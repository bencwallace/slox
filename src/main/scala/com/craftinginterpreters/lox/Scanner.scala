package com.craftinginterpreters.lox

import scala.collection.mutable.ArrayBuffer

object Scanner {
  val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
}

class Scanner(val source: String) {

  private var start = 0
  private var current = 0
  private var line = 1
  private val tokens = ArrayBuffer[Token]()

  def scanTokens(): Seq[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken()
    }
    tokens += Token(EOF, "", None, line)
    tokens.toSeq
  }

  private def scanToken(): Unit = advance() match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if (matchToken('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (matchToken('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (matchToken('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (matchToken('=')) GREATER_EQUAL else GREATER)
      case '/' =>
        if (matchToken('/')) while(peek != '\n' && !isAtEnd) advance()
        else addToken(SLASH)
      case ' ' | '\r' | '\t' => ()
      case '\n' => line += 1
      case '"' => string()
      case c @ _ =>
        if (isDigit(c)) number()
        else if (isAlpha(c)) identifier()
        else Lox.error(line, "Unexpected character.")
  }

  // utility methods

  private def addToken(tokenType: TokenType): Unit = addToken(tokenType, None)

  private def addToken(tokenType: TokenType, literal: Option[Value]): Unit = {
    val text = source.substring(start, current)
    tokens += Token(tokenType, text, literal, line)
  }

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def isAtEnd: Boolean = current >= source.length

  private def matchToken(expected: Char): Boolean =
    if (isAtEnd || source.charAt(current) != expected) false
    else {
        current += 1
        true
      }

  private def peek: Char = if (isAtEnd) '\u0000' else source.charAt(current)

  private def peekNext: Char = if (current + 1 >= source.length) '\u0000' else source.charAt(current + 1)

  // specialized lexers

  private def identifier(): Unit = {
    while (isAlphaNumeric(peek)) advance()

    val text = source.substring(start, current)
    val tokenType = Scanner.keywords.get(text) match {
      case None => IDENTIFIER
      case Some(t) => t
    }

    addToken(tokenType)
  }

  private def number(): Unit = {
    while (isDigit(peek)) advance()

    if (peek == '.' && isDigit(peekNext)) {
      advance()
      while (isDigit(peek)) advance()
    }

    addToken(NUMBER, Some(Number(source.substring(start, current).toDouble)))
  }

  private def string(): Unit = {
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') line += 1
      advance()
    }

    // todo: does this return?
    if (isAtEnd) Lox.error(line, "Unterminated string.")
    else {
      // consume closing quote
      advance()

      // trim surrounding quotes
      val value = source.substring(start + 1, current - 1)
      addToken(STRING, Some(Str(value)))
    }
  }

  // character range checks

  private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

}
