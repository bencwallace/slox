package com.craftinginterpreters.lox

import scala.collection.mutable.Map

class Environment(enclosing: Option[Environment] = None) {
  private val values = Map[String, Value]()

  def assign(token: Token, value: Value): Unit = (values.get(token.lexeme), enclosing) match {
    case (Some(_), _) => values += (token.lexeme -> value)
    case (None, Some(env)) => env.assign(token, value)
    case (None, None) => throw RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
  }

  def define(name: String, value: Value): Unit = values += (name -> value)

  def getAt(distance: Int, name: String): Value = ancestor(distance) match {
    case None => ???
    case Some(e) => e.values(name)
  }

  def assignAt(distance: Int, token: Token, value: Value): Unit = ancestor(distance) match {
    case None => ???
    case Some(e) => e.values += (token.lexeme -> value)
  }

  def get(token: Token): Value = (values.get(token.lexeme), enclosing) match {
      case (Some(value), _) => value
      case (None, Some(env)) => env.get(token)
      case (None, None) => throw RuntimeError(token, s"Undefined variable '${token.lexeme}'.")
    }

  private def ancestor(distance: Int): Option[Environment] =
    if (distance == 0) Some(this)
    else enclosing.flatMap(e => e.ancestor(distance - 1))
}
