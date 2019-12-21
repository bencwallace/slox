package com.craftinginterpreters.lox

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

class Parser(tokens: Seq[Token]) {

  var current = 0

  private case class ParseError() extends RuntimeException

  def parse(): Queue[Stmt] = {
    @tailrec
    def parseRec(acc: Queue[Stmt]): Queue[Stmt] = {
      val statement = topLevelParser()
      statement match {
        case End => acc
        case _ => parseRec(acc.enqueue(statement))
      }
    }
    parseRec(Queue[Stmt]())
  }

  // todo: update as appropriate
  private def topLevelParser(): Stmt =
    if (isAtEnd) End
    else declaration()

  // declaration parsers

  private def declaration(): Stmt =
    try {
      if (matchTokens(VAR)) varDeclaration()
      else statement()
    } catch {
      case error: ParseError => {
        synchronize()
        topLevelParser()
      }
    }

  private def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect variable name.")
    val init = if (matchTokens(EQUAL)) Some(expression()) else None
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, init)
  }

  // statement parsers

  private def statement(): Stmt =
    if (matchTokens(PRINT)) printStatement()
    else if (matchTokens(LEFT_BRACE)) Block(block())
    else expressionStatement()

  private def printStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(expr)
  }

  private def block(): Seq[Stmt] = {
    val statements = mutable.Queue[Stmt]()

    while (!isAtEnd && !check(RIGHT_BRACE))
      statements.enqueue(declaration())

    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements.toSeq
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  // expression parsers

  private def expression(): Expr = assignment()

  private def assignment(): Expr = {
    val left = equality()

    if (matchTokens(EQUAL)) {
      val equals = previous
      val right = assignment()

      left match {
        case Variable(token) => Assign(token, right)
        case _ => {
          error(equals, "Invalid assignment target.")
          left
        }
      }
    } else left
  }

  private def binary(tokenTypes: TokenType*)(nextParser: () => Expr): Expr = {
    var expr = nextParser()
    while (matchTokens(tokenTypes:_*)) {
      val operator = previous
      val right = nextParser()
      expr = Binary(expr, operator, right)
    }
    expr
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

  // utility methods (pure)

  private def check(tokenType: TokenType): Boolean =
    if (isAtEnd) false else peek.tokenType == tokenType

  private def isAtEnd: Boolean = peek.tokenType == EOF

  private def peek: Token = tokens(current)

  private def previous = tokens(current - 1)

  // utility methods (with side-effects)

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

  private def consume(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) advance()
    else {
      throw error(peek, message)
    }

  // error handling

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    ParseError()
  }

  // todo: fix
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
