## Things done after return/try-catch-throw

* Make programming in the language easier
  * Added unary boolean/integer negation
  * Removed the need for an `else` statement in `if`
  * The whole program is in a try-catch block that will print out the error
* Added the `-d` flag to main to see the stacktrace (Same as `uncommenting step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined`)
* When displaying environments primitives are not displayed for ease of reading
