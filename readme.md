## Things done after return/try-catch-throw

* Make programming in the language easier
  * Added unary boolean/integer negation
  * Removed the need for an `else` statement in `if`
  * The whole program is in a try-catch block that will print out the error
* Added the `-d` flag to main to see the stacktrace (Same as `uncommenting step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined`)
* When displaying environments primitives are not shown for ease of debugging
* Import other files (for now kinda rude)
  * The path to the imported file is the relative path from where you are executing `.\Main` (Not the path of main itself!)
  * It used unsafe IO actions (Will try to remove them if possible)
  * Does not print out imported file with `-v`. (but does with `-d`)
  * It works by executing the file normally then appending the returned environment to the current environment
  * Note: The whole file are executed so any function call will still run
* Added support for multithreading
  * Syntax differ from example given in assignment notes. You now do not need to declare a function after a spawn function.
  * Spawn support all expressions
  * All threads are covered by a try-catch block (like the main program)
* for-loops
  * Essentially while loops with declaration, just sugar syntax 