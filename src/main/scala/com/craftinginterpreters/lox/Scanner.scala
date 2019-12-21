package com.craftinginterpreters.lox

import scala.annotation.tailrec

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

  def scanTokens(): Seq[Token] = {
    @tailrec
    def scanTokensRec(acc: Seq[Token]): Seq[Token] = {
      val token = scanToken()
      token match {
        case Token(EOF, _, _, _) => acc :+ token
        case _ => scanTokensRec(acc :+ token)
      }
    }
    // todo: replaced IndexedSeq by ArraySeq or ArrayBuffer
    scanTokensRec(IndexedSeq())
  }

  @tailrec
  private def scanToken(): Token =
    if (!isAtEnd) {
      start = current
      advance() match {
        case '(' => makeToken(LEFT_PAREN)
        case ')' => makeToken(RIGHT_PAREN)
        case '{' => makeToken(LEFT_BRACE)
        case '}' => makeToken(RIGHT_BRACE)
        case ',' => makeToken(COMMA)
        case '.' => makeToken(DOT)
        case '-' => makeToken(MINUS)
        case '+' => makeToken(PLUS)
        case ';' => makeToken(SEMICOLON)
        case '*' => makeToken(STAR)
        case '!' => makeToken(if (matchToken('=')) BANG_EQUAL else BANG)
        case '=' => makeToken(if (matchToken('=')) EQUAL_EQUAL else EQUAL)
        case '<' => makeToken(if (matchToken('=')) LESS_EQUAL else LESS)
        case '>' => makeToken(if (matchToken('=')) GREATER_EQUAL else GREATER)
        case '/' =>
          if (matchToken('/')) {
            while (peek != '\n' && !isAtEnd) advance()
            scanToken()
          }
          else makeToken(SLASH)
        case ' ' | '\r' | '\t' => {
          scanToken()
        }
        case '\n' => {
          line += 1
          scanToken()
        }
        case '"' => string()
        case c =>
          if (isDigit(c)) number()
          else if (isAlpha(c)) identifier()
          else {
            Lox.error(line, "Unexpected character.")
            scanToken()
          }
      }
    } else Token(EOF, "", None, line)

  // tokenizers

  private def makeToken(tokenType: TokenType): Token = makeToken(tokenType, None)

  private def makeToken(tokenType: TokenType, literal: Option[Value]): Token = {
    val text = source.substring(start, current)
    Token(tokenType, text, literal, line)
  }

  private def identifier(): Token = {
    while (isAlphaNumeric(peek)) advance()

    val text = source.substring(start, current)
    val tokenType = Scanner.keywords.get(text) match {
      case None => IDENTIFIER
      case Some(t) => t
    }

    makeToken(tokenType)
  }

  private def number(): Token = {
    while (isDigit(peek)) advance()

    if (peek == '.' && isDigit(peekNext)) {
      advance()
      while (isDigit(peek)) advance()
    }

    makeToken(NUMBER, Some(Number(source.substring(start, current).toDouble)))
  }

  private def string(): Token = {
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') line += 1
      advance()
    }

    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
      scanToken()
    }
    else {
      // consume closing quote
      advance()

      // trim surrounding quotes
      val value = source.substring(start + 1, current - 1)
      makeToken(STRING, Some(Str(value)))
    }
  }

  // utility methods (pure)

  private def isAtEnd: Boolean = current >= source.length

  private def peek: Char = if (isAtEnd) '\u0000' else source.charAt(current)

  private def peekNext: Char = if (current + 1 >= source.length) '\u0000' else source.charAt(current + 1)

  private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  private def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

  // utility methods with side-effects

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def matchToken(expected: Char): Boolean =
    if (isAtEnd || source.charAt(current) != expected) false
    else {
        current += 1
        true
      }

}
