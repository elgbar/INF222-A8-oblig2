module Main where

import Control.Monad      (when)
import Data.Either
import Eval
import Parser
import Pretty
import Syntax
import System.Environment (getArgs)


main = do
  args <- getArgs
  let verbose = "-v" `elem` args
  let debug = "-d" `elem` args
  let code = "-c" `elem` args
  let interpreter = "-i" `elem` args
  let cleanedArgs = filter (\e -> e `notElem` ["-v", "-d", "-c", "-i"]) args
  if interpreter then 
    runInterp [] verbose debug code
  else do
    let fname =
          if null cleanedArgs
            then error "no filename"
            else head cleanedArgs
    input <- readFile fname
    run input fname [] verbose debug code
  return ()

