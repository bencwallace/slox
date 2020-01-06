# slox

A Scala port of [jlox](https://github.com/munificent/craftinginterpreters/tree/master/java),
the Java implementation of a tree-walk interpreter for the *Lox* language,
from the excellent online book [Crafting Interpreters](https://craftinginterpreters.com/).
I **strongly** recommend Part II of the book for understanding the design of the Lox language
and the jlox interpreter and I look forward to reading Part III.

The overall structure follows the design in the book,
but takes advantage of some of Scala's functional features where appropriate. Most notably,
the visitor pattern, used extensively in jlox, is replaced by pattern matching in slox
(indeed, the book's [author](https://github.com/munificent) even suggests this).

## Why Scala?

1. To learn Scala
2. Easy to port from Java
3. It isn't (at the time of writing) on the
[list](https://github.com/munificent/craftinginterpreters/wiki/Lox-implementations)
of jlox ports.
4. Its functional nature makes it natural for writing an interpreter or compiler.

## Design notes

* Pattern matching with case classes and sealed traits replaces the visitor pattern as well as uses of `instanceOf`.
* Lox values are stored as `Value` objects rather than `Object` objects.
* The `Scanner` and `Parser` are implemented as tail-recursive functions.
* Binary operator parsers are implemented using a single higher-order function.

## Conclusion

Scala is truly an ideal language for implementing an interpreter, taking advantage of both
algebraic data structures (representing and processing abstract syntax trees) and
objects (for storing and manipulating mutable state, e.g. current parser position).
The resulting source is also considerably shorter (some parts of the original source are
sufficiently long and repetitive that they are generated using an auxiliary source file,
but this is not necessary in Scala).
