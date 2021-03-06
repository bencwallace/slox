package com.craftinginterpreters.lox

import java.io.FileNotFoundException

import scala.io.{Source, StdIn}

object Lox {

  private var hadError = false
  private var hadRuntimeError = false

  def main(args: Array[String]): Unit =
    if (args.length > 1) {
      println("Usage: slox [script]")
      sys.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt();
    }

  private def run(source: String): Unit =
    if (source == null) sys.exit(66)
    else {
      val scanner = new Scanner(source)
      val tokens = scanner.scanTokens()

      val parser = new Parser(tokens)
      val statements = parser.parse()

      if (hadError || hadRuntimeError) println()

      Resolver.resolve(statements)

      if (hadError) return

      Interpreter.interpret(statements)
    }

  private def runPrompt(): Unit =
    while(true) {
      print("> ")
      run(StdIn.readLine())
      hadError = false
      hadRuntimeError = false
    }

  private def runFile(path: String): Unit =
    try {
      run(Source.fromFile(path).mkString)
      if (hadError) sys.exit(65)
      if (hadRuntimeError) sys.exit(70)
    } catch {
      case _: FileNotFoundException => println("Source file not found.")
    }

  // runtime errors

  private[lox] def runtimeError(error: RuntimeError): Unit = {
    System.err.println(s"${error.message}\n[line ${error.token.line}]")
    hadRuntimeError = true
  }

  // static errors

  private[lox] def error(line: Int, message: String): Unit = report(line, "", message)

  private[lox] def error(token: Token, message: String): Unit = token match {
    case Token(EOF) => report(token.line, " at end", message)
    case _ => report(token.line, s" at '${token.lexeme}'", message)
  }

  // error reporting

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

}
