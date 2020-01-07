# slox

A Scala port of [jlox](https://github.com/munificent/craftinginterpreters/tree/master/java),
the Java implementation of a tree-walk interpreter for the *Lox* language,
from the excellent online book [Crafting Interpreters](https://craftinginterpreters.com/)
by Bob Nystrom.
I **strongly** recommend Part II of the book for understanding the design of the Lox language
and the jlox interpreter and I look forward to reading Part III.

The overall structure follows the design in the book,
but takes advantage of some of Scala's functional features where appropriate. Most notably,
the visitor pattern, used extensively in jlox, is replaced by pattern matching in slox
(indeed, the book's author hints at this).

## Why Scala?

1. To learn Scala
2. Natural to port from Java
3. It wasn't on the
[list](https://github.com/munificent/craftinginterpreters/wiki/Lox-implementations)
of jlox ports.
4. Its functional nature makes it natural for writing an interpreter or compiler.

## Conclusion

Scala is truly an ideal language for implementing an interpreter, taking advantage of both
algebraic data structures (for representing and processing abstract syntax trees) and
objects (for storing and manipulating mutable state, e.g. current parser position).
The resulting source is also considerably shorter then the original jlox source (some parts
of the original source are sufficiently long and repetitive that they are generated using an
auxiliary source file, but this is not necessary in Scala).

## Misc

### Design notes

* Pattern matching with case classes and sealed traits replaces the visitor pattern as well as uses of `instanceOf`.
* Lox values are stored as `Value` objects rather than `Object` objects.
* The `Scanner` and `Parser` are implemented as tail-recursive functions.
* Binary operator parsers are implemented using a single higher-order function.

### Bugs

In the following test files from [munificent](https://github.com/munificent/craftinginterpreters/tree/master/test):

* closure/reuse_closure_slot.lox
* for/closure_in_body.lox
* for/scope.lox
* for/syntax.lox
* regression/40.lox (when returning `nil`)
