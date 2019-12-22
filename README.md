# slox

**!Work in progress!**

A port of [jlox](https://github.com/munificent/craftinginterpreters/tree/master/java),
the Java implementation of a tree-walk interpreter for the *Lox* language,
from the online book [Crafting Interpreters](https://craftinginterpreters.com/).

The design follows that in the book for the most part, making use of Scala's functional capabilities when
appropriate.

## Why Scala?

1. To learn Scala
2. Easy to port from Java
3. It isn't (at the time of writing) on the [list](https://github.com/munificent/craftinginterpreters/wiki/Lox-implementations)
of jlox ports.
4. Its functional nature makes it natural for writing an interpreter or compiler.

### Pattern matching

Scala's *case classes* allow algebraic data types to coexist with a traditional OOP framework.
Pattern matching on these data types obviate the need for an implementation of the *Visitor pattern*,
as explained in the book. In particular, they allow for the following simplifications:

* `AstPrinter` does not need the visitor pattern;
* `Expr` is much shorter;
* `GenerateAst` can be removed entirely;
* `Interpreter` does not need the visitor pattern, switch statements, `instanceOf`,
or explicit casts, and runtime errors are easy to catch with the wildcard pattern;
* `Parser` does not need the visitor pattern, and
* `Stmt` is much shorter

Pattern matching also encourage the definition of a `Value` type to represent Lox values,
rather than storing these values as `Object`s. This helps avoid typical pitfalls of working
directly with `Object`.

### Design notes

* Lox values are stored as `Value` objects rather than `Object` objects
* Lexical scope is implemented by passing the local environment to a new `Interpreter` object
* The visitor pattern is replaced by pattern matching
* The `Scanner` and `Parser` are implemented as tail-recursive functions
* Binary operator parsers are implemented using higher-order functions
