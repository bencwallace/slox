package com.craftinginterpreters.lox

import scala.collection.mutable.Map

class Environment {
  private val values = Map[String, Value]()

  def assign(token: Token, value: Value): Unit = values.get(token.lexeme) match {
    case Some(_) => values += (token.lexeme -> value)
    case None => throw RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
  }

  def define(name: String, value: Value): Unit = values += (name -> value)

  def get(token: Token): Value = {
    val temp = values.get(token.lexeme)
    temp match {
      case Some(value) => value
      case None => throw RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
    }
  }
}
