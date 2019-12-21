package com.craftinginterpreters.lox

import scala.collection.mutable.Map

class Environment {
  private val values = Map[String, Value]()

  def define(name: String, value: Value): Unit = values += (name -> value)

  def get(token: Token): Value = values.get(token.lexeme) match {
      case Some(value) => value
      case None => throw RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
    }
}
