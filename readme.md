## Things done after return/try-catch-throw


* Added the `-d` flag to main to see the stacktrace (Same as `uncommenting step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined`)
* When displaying environments primitives are not shown for ease of debugging
* Make programming in the language easier
  * Added unary boolean/integer negation
  * Removed the need for an `else` statement in `if`
  * The whole program is in a try-catch block that will print out the error
  * Essentially while loops with declaration, just sugar syntax 
* Import other files (for now kinda rude)
  * The path to the imported file is the relative path from where you are executing `.\Main` (Not the path of main itself!)
  * It used unsafe IO actions (Will try to remove them if possible)
  * Does not print out imported file with `-v`. (but does with `-d`)
  * It works by executing the file normally then appending the returned environment to the current environment
  * Note: The whole file are executed so any function call will still run
* Multi threading
  * Syntax differ from example given in assignment notes. You now do not need to declare a function after a spawn function.
  * Spawn support all expressions
  * All threads are covered by a try-catch block (like the main program)
* A simple type system
  * Can declare types of constants and references, but cannot with primitive functions ie `int y = x + 1;` (though `var y = x + 1;` will still work)
  * Cannot change the type of any variable (no matter how it is declared)
  * bug: Functions can still return multiple different types of values