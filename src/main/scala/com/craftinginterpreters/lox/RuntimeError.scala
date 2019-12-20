package com.craftinginterpreters.lox

case class RuntimeError(token: Token, message: String) extends RuntimeException
