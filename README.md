## Advantages of Scala

* Case classes
  * TokenType
  * Expr (much shorter, no need for GenerateAst)
  * Token (clearly shows advantage of case classes over, e.g. enums)
* Pattern matching -- replaces visitor pattern
  * AstPrinter (no visitor pattern)
  * Interpreter (no visitor pattern or switch statement)
    * No `instanceOf` or explicit casts
    * Error check with wildcard
* Higher-order functions
  * Parser

## Changes to note

* Use `None` instead of `null`
* Changed `evaluate` to `eval`

## To do

* look into using laziness
  * e.g. "interlace" lexing and parsing 
  * http://matt.might.net/articles/implementation-of-lazy-list-streams-in-scala/
  * http://matt.might.net/articles/implementing-laziness/
  * https://scala-lang.org/blog/2017/11/28/view-based-collections.html
