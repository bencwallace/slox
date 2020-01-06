# slox

**!Work in progress!**

A port of [jlox](https://github.com/munificent/craftinginterpreters/tree/master/java),
the Java implementation of a tree-walk interpreter for the *Lox* language,
from the online book [Crafting Interpreters](https://craftinginterpreters.com/).

The overall structure of the ported interpreter follows the design of that in the book,
but takes advantage of some of Scala's functional features where appropriate. Most notably,
the visitor pattern, used extensively in jlox, is replaced by pattern matching in slox.

## Why Scala?

1. To learn Scala
2. Easy to port from Java
3. It isn't (at the time of writing) on the
[list](https://github.com/munificent/craftinginterpreters/wiki/Lox-implementations)
of jlox ports.
4. Its functional nature makes it natural for writing an interpreter or compiler.

## Design notes

* The visitor pattern is replaced by pattern matching with case classes and sealed traits.
* Lox values are stored as `Value` objects rather than `Object` objects.
* The `Scanner` and `Parser` are implemented as tail-recursive functions.
* Binary operator parsers are implemented using a single higher-order function.
