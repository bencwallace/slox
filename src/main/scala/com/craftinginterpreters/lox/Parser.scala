package com.craftinginterpreters.lox

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  // todo: temporary; update as appropriate
  private def topLevelParser(): Stmt =
    if (isAtEnd) End
    else declaration()

  // declaration parsers

  private def declaration(): Stmt =
    try {
      if (matchTokens(FUN)) function("function")
      else if (matchTokens(VAR)) varDeclaration()
      else statement()
    } catch {
      case _: ParseError => {
        synchronize()
        topLevelParser()
      }
    }

  // function declaration
  private def function(kind: String): Stmt = {
    @tailrec
    def paramsRec(acc: ArrayBuffer[Token]): ArrayBuffer[Token] =
      if (matchTokens(COMMA)) {
        if (acc.size >= 255)
          error(peek, "Cannot have more than 255 parameters.")
          paramsRec(acc :+ consume(IDENTIFIER, "Expect parameter name."))
      }
      else acc

    // parse name
    val name = consume(IDENTIFIER, s"Expect $kind name.")
    consume(LEFT_PAREN, s"Expect '(' after $kind name.")

    // parse parameter names
    val firstParams =
      if (!check(RIGHT_PAREN)) ArrayBuffer(consume(IDENTIFIER, "Expect parameter name."))
      else ArrayBuffer[Token]()
    val params = paramsRec(firstParams)
    consume(RIGHT_PAREN, "Expect ')' after parameters.")

    // parse body
    consume(LEFT_BRACE, s"Expect '{' before $kind body.")
    val body = block()

    Function(name, params.toSeq, body)
  }

  private def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect variable name.")
    val init = if (matchTokens(EQUAL)) Some(expression()) else None
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, init)
  }

  // statement parsers

  private def statement(): Stmt =
    if (matchTokens(LEFT_BRACE)) Block(block())
    else if (matchTokens(IF)) ifStatement()
    else if (matchTokens(FOR)) forStatement()
    else if (matchTokens(PRINT)) printStatement()
    else if (matchTokens(WHILE)) whileStatement()
    else expressionStatement()

  private def block(): Seq[Stmt] = {
    @tailrec
    def blockRec(acc: Queue[Stmt]): Queue[Stmt] =
      if (!isAtEnd && !check(RIGHT_BRACE))
        blockRec(acc :+ declaration())
      else {
        consume(RIGHT_BRACE, "Expect '}' after block.")
        acc
      }
    blockRec(Queue())
  }

  private def ifStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch = statement()
    val elseBranch = if (matchTokens(ELSE)) Some(statement()) else None

    If(condition, thenBranch, elseBranch)
  }

  // desugar for loop to while loop
  private def forStatement(): Stmt = {
    // parse initializer, condition, and increment
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer =
      if (matchTokens(SEMICOLON)) None
      else if (matchTokens(VAR)) Some(varDeclaration())
      else Some(expressionStatement())

    val condition =
      if (!check(SEMICOLON)) expression()
      else Literal(Bool(true))
    consume(SEMICOLON, "Expect ';' after loop condition.")

    val increment =
      if (!check(RIGHT_PAREN)) Some(expression())
      else None
    consume(RIGHT_PAREN, "Expect ')', after for clauses.")

    // desugar
    val whileLoop = While(condition, increment match {
      case Some(inc) => Block(Seq(statement(), Expression(inc)))
      case None => Block(Seq(statement()))
    })

    Block(initializer match {
      case Some(init) => Seq(init, whileLoop)
      case None => Seq(whileLoop)
    })
  }

  private def printStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(expr)
  }

  private def whileStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement()

    While(condition, body)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  // expression parsers

  private def expression(): Expr = assignment()

  private def assignment(): Expr = {
    val left = or()

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

  // template for binary operator expression parsers
  private def binary(tokenTypes: TokenType*)(nextParser: () => Expr): Expr = {
    var expr = nextParser()
    while (matchTokens(tokenTypes:_*)) {
      val operator = previous
      val right = nextParser()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def or(): Expr = binary(OR)(and)

  private def and(): Expr = binary(AND)(equality)

  private def equality(): Expr = binary(BANG_EQUAL, EQUAL_EQUAL)(comparison)

  private def comparison(): Expr = binary(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)(addition)

  private def addition(): Expr = binary(MINUS, PLUS)(multiplication)

  private def multiplication(): Expr = binary(SLASH, STAR)(unary)

  private def unary(): Expr =
    if (matchTokens(BANG, MINUS)) {
      val operator = previous
      val right = unary()
      Unary(operator, right)
    } else call()

  // method and function call parsers
  private def call(): Expr = {
    var expr = primary()

    // todo: different from book -- beware extension
    while (matchTokens(LEFT_PAREN))
      expr = finishCall(expr)

    expr
  }

  // function call parser
  private def finishCall(callee: Expr): Expr = {
    val args = new ListBuffer[Expr]()
    if (!check(RIGHT_PAREN))
      do {
        if (args.size >= 255)
          error(peek, "Cannot have more than 255 arguments.")
        args += expression()
      } while (matchTokens(COMMA))
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")

    Call(callee, paren, args.toSeq)
  }

  // primary (leaf) parser
  private def primary(): Expr =
    if (matchTokens(FALSE)) Literal(Bool(false))
    else if (matchTokens(TRUE)) Literal(Bool(true))
    else if (matchTokens(NIL)) Literal(NilVal)
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
    else throw error(peek, message)

  // error handling

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    ParseError()
  }

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
