## Things done after return/try-catch-throw


* Added the `-d` flag to main to see the stacktrace (Same as `uncommenting step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined`)
* When displaying environments primitives are not shown for ease of debugging
* Make programming in the language feel nicer
  * Added unary boolean/integer negation.
  * Integers can be declared as negative numbers.
  * Removed the need for an `else` in `if` statement.
  * The whole program and all threads are surrounded by a try-catch block that will print out any uncaught `throws`.
  * Added for-loops
    * Essentially while loops with declaration, just sugar syntax.
* Import other files
  * The path to the imported file is the relative path from where you are executing `.\Main` (Not the path of main itself!)
  * It used unsafe IO actions
    * Should not be used in an ideal world, but i've not managed to make the import work without it.
  * Does not print out imported file with `-v`. (but will do so if `-d` is present)
  * It works by executing the file normally then appending the returned environment to the current environment
  * Note: The whole file are executed so any function call will still run
* Multi threading
  * Syntax differ from example given in assignment notes. You now do not need to declare a function after a spawn function.
  * Spawn support all expressions
  * All threads are covered by a try-catch block (like the main program)
* A simple type system
  * Cannot change the type of any variable (no matter how it is declared)
  * Can declare types of constants and references, but cannot with primitive functions ie `int y = x + 1;` (though `var y = x + 1;` will still work)
  * bug: Functions can still return multiple different types of values

## Run all examples

For ease of testing (for the both of us) i've created a script called `runall.sh` that will run every `.impf` file found under `./examples`. To run it properly (because of imports) execute it while standing in the folder it is in.