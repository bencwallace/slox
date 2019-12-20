# slox

**!Work in progress!**

A port of [jlox](https://github.com/munificent/craftinginterpreters/tree/master/java/com/craftinginterpreters),
the Java implementation of a tree-walk interpreter for the *Lox* language,
from the online book [Crafting Interpreters](https://craftinginterpreters.com/).

## Why Scala?

Besides being relatively straightforward to port from Java, Scala is a natural language for
writing an interpreter or compiler due to its functional nature. 

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

### Higher-order functions

Higher-order functions are used as follows:

* `Parser` can be simplified by refactoring the various binary expression parsers.
