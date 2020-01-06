package com.craftinginterpreters.lox

class LoxInstance(klass: LoxClass) {
  override def toString: String = s"${klass.toString} instance"
}
