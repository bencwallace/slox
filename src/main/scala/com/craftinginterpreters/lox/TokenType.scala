package com.craftinginterpreters.lox

sealed trait TokenType

case object LEFT_PAREN extends TokenType
case object RIGHT_PAREN extends TokenType
case object LEFT_BRACE extends TokenType
case object RIGHT_BRACE extends TokenType
case object COMMA extends TokenType
case object DOT extends TokenType
case object MINUS extends TokenType
case object PLUS extends TokenType
case object SEMICOLON extends TokenType
case object SLASH extends TokenType
case object STAR extends TokenType

case object BANG extends TokenType
case object BANG_EQUAL extends TokenType
case object EQUAL extends TokenType
case object EQUAL_EQUAL extends TokenType
case object GREATER extends TokenType
case object GREATER_EQUAL extends TokenType
case object LESS extends TokenType
case object LESS_EQUAL extends TokenType

case object IDENTIFIER extends TokenType
case object STRING extends TokenType
case object NUMBER extends TokenType

case object AND extends TokenType
case object CLASS extends TokenType
case object ELSE extends TokenType
case object FALSE extends TokenType
case object FUN extends TokenType
case object FOR extends TokenType
case object IF extends TokenType
case object NIL extends TokenType
case object OR extends TokenType
case object PRINT extends TokenType
case object RETURN extends TokenType
case object SUPER extends TokenType
case object THIS extends TokenType
case object TRUE extends TokenType
case object VAR extends TokenType
case object WHILE extends TokenType

case object EOF extends TokenType
