module Main where

import Syntax
import Parser
import Pretty
import Eval
import Data.Either
import System.Environment (getArgs)
import Control.Monad (when)

main = do
  args <- getArgs
  let verbose = "-v" `elem` args
  let cleanedArgs = filter (/= "-v") args
  let fname = if null cleanedArgs then error "no filename" else head cleanedArgs
  input <- readFile fname
  case parse program fname input of  
    Right v -> do
      when (verbose) $ putStrLn $ pPrint v
      exec v >> return ()
    Left e -> print e
