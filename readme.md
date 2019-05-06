## Things done after return/try-catch-throw

* Make programming in the language easier
  * Added unary boolean/integer negation
  * Removed the need for an `else` statement in `if`
  * The whole program is in a try-catch block that will print out the error
* Added the `-d` flag to main to see the stacktrace (Same as `uncommenting step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined`)
* When displaying environments primitives are not displayed for ease of reading
* Import other files (for now kinda rude)
  * The path to the imported file is the relative path from where you are executing `.\Main` (Not the path of main itself!)
  * It used unsafe IO actions (Will try to remove them if possible)
  * Does not print out imported file with `-v`. (but does with `-d`)
  * It works by executing the file normally then moving returning the saved environment

## TODO

* for-loops