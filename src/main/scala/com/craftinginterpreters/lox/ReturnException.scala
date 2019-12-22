package com.craftinginterpreters.lox

case class ReturnException(value: Value) extends RuntimeException(null, null, false, false)
