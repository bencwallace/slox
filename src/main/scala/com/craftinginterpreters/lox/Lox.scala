package com.craftinginterpreters.lox

import scala.io.{Codec, Source, StdIn}
import scala.util.Using

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

  // todo: fix this on ctrl+d
  private def run(source: String): Unit =
    if (source == null) sys.exit(66)

    else {
      val scanner = new Scanner(source)
      val tokens = scanner.scanTokens()

      val parser = new Parser(tokens)
      val statements = parser.parse()

      if (hadError || hadRuntimeError) println()
      else Interpreter.interpret(statements)
    }

  private def runPrompt(): Unit =
    while(true) {
      print("> ")
      run(StdIn.readLine())
      hadError = false
      hadRuntimeError = false // todo
    }

  private def runFile(path: String): Unit =
    Using(Source.fromFile(path)(Codec.defaultCharsetCodec)) { source =>
      run(source.mkString)
      if (hadError) sys.exit(65)
      if (hadRuntimeError) sys.exit(70)
    }

  // runtime errors

  private[lox] def runtimeError(error: RuntimeError): Unit = {
    System.err.println(error.message + "\n[line " + error.token.line + "]")
    hadRuntimeError = true
  }

  // static errors

  private[lox] def error(line: Int, message: String): Unit = report(line, "", message)

  private[lox] def error(token: Token, message: String): Unit =
    if (token.tokenType == EOF) report(token.line, " at end", message)
    else report(token.line, " at '" + token.lexeme + "'", message)

  // error reporting

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }

}
