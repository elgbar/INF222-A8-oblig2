module Primitive where

import Control.Monad
import Syntax
import Parser
import Data.IORef
import System.IO
import Data.Maybe
import Text.Read
import Control.Exception

primitives :: Env
primitives =
  [ ("__u-", VPrimFun "-" $ \[VInt x] -> VInt $ -x)
  , ("__u!", VPrimFun "!" $ \[VBool x] -> VBool $ not x)
  , ("__up++", VPrimFunIO "++" $ \[r@(VRef ref (VInt _))] -> do
                (VInt i) <-readIORef ref
                writeIORef ref $ VInt $ i+1
                return r
    )
  , ("__u--", VPrimFun "--" $ \[VInt x] -> VInt $ x - 1)
  , ("__b+", VPrimFun "+" $ \args ->
          case args of 
            [VInt x, VInt y] -> VInt $ x + y
            [VString x, VString y] -> VString $ x ++ y)
  , ("__b-", VPrimFun "-" $ \[VInt x, VInt y] -> VInt $ x - y)
  , ("__b*", VPrimFun "*" $ \[VInt x, VInt y] -> VInt $ x * y)
  , ("__b/", VPrimFun "/" $ \[VInt x, VInt y] -> VInt $ x `div` y)
  , ("__b%", VPrimFun "%" $ \[VInt x, VInt y] -> VInt $ x `mod` y)
  , ("__b==", VPrimFun "==" $ \args ->
          case args of 
            [VArr es1, VArr es2] -> VBool $ (VArr (readRefs es1)) == (VArr (readRefs es2))
            [e1, e2] -> VBool $ e1 == e2)
  , ("__b!=", VPrimFun "!=" $ \args ->
          case args of 
            [VArr es1, VArr es2] -> VBool $ (VArr (readRefs es1)) /= (VArr (readRefs es2))
            [e1, e2] -> VBool $ e1 /= e2)
  , ("__b<", VPrimFun "<" $ \[VInt x, VInt y] -> VBool $ x < y)
  , ("__b<=", VPrimFun "<=" $ \[VInt x, VInt y] -> VBool $ x <= y)
  , ("__b>", VPrimFun ">" $ \[VInt x, VInt y] -> VBool $ x > y)
  , ("__b>=", VPrimFun ">="$ \[VInt x, VInt y] -> VBool $ x >= y)
  , ("__b&&", VPrimFun "&&"$ \[VBool x, VBool y] -> VBool $ x && y)
  , ("__b||", VPrimFun "||"$ \[VBool x, VBool y] -> VBool $ x || y)
  -- array functions
  , ("length", VPrimFunIO "length" $ \args -> -- the length of an array
      case args of 
        [VRef rh _] -> do -- allow for reference arrays to 
          (VArr xs) <- readIORef rh
          return $ VInt $ length xs
        [VArr xs] -> return $ VInt $ length xs
        [VString xs] -> return $ VInt $ length xs
        )
  , ("add", VPrimFunIO "add"$ \args -> -- return a new array with the added element
      case args of 
        [VRef rh _, e] -> do
          VArr xs <- readIORef rh
          return $ VArr $ xs ++ [EVal e]) 
  , ("insert", VPrimFunIO "insert" $ \args -> -- insert an element at the given index
      case args of  
        [VRef rh _, e, VInt index] -> do
          VArr xs <- readIORef rh
          let (a,b) = splitAt index xs
          return $ VArr $ a ++ (EVal e : b))                  
  , ("remove", VPrimFunIO "remove"$ \args ->
            case args of 
              [VRef rh (VArr _), VInt index] -> do
                read <- readIORef rh
                let xs = case read of 
                            VArr ys -> ys
                            _ -> return $ EVal $ VException $ "can only remove elements from arrays"
                if index <= 0 then return $ VArr $ if null xs then [] else tail xs
                else do
                  let (a,b) = splitAt (index+1) xs
                  return $ VArr $ safeInit a ++ b)
  , ("create", VPrimFun "create" $ \[VInt size] -> VArr $ replicate size $ ERef $ EVal VVoid) -- create an array with the given size and void as elemt
  , ("toString", 
      VPrimFunIO "toString" $ \args -> -- print out arrays as a string (no spaces between elements)
        case args of
            [VRef rh _] -> do
              val <- readIORef rh
              let (VPrimFunIO _ f) = fromJust (lookup "toString" primitives)
              f [val]
            [VArr xs] -> return $ VString $ concatMap (\ev -> case ev of{EVal v -> show v;_ -> show ev}) (readRefs xs)
            [v] -> return $ VString $ show v)
  -- , ("parse", VPrimFunIO "parse" (\[VString str] -> 
  --         case parse oVals "<parse>" str of
  --           Right e -> case e of
  --                   EVal v -> return v
  --                   e -> error $ "Given string does not result in an value "++ show str++"\nbut rather "++show e
  --           Left e -> error ("Failed to parse given string "++ show str) >>= \r -> print e >> return r)
  --   )
  , ("fromString", VPrimFun "fromString" $ \[VString s] ->
                case (readMaybe::String->Maybe Value) s of
                    Just e -> e
                    Nothing -> VException "Failed to convert the string to a value"
    )
  , ("typeof", VPrimFun "typeof" $ \[e] -> VString $ val2type e)
  , ("readln", VPrimFunIO "readln" $ \[] -> getLine >>= \inp -> return $ VString inp)
  , ("print", VPrimFunIO "print" $ \args -> mapM_ (putStr . show) args >> hFlush stdout >> return VVoid)
  , ("println" , VPrimFunIO "println" $ \args -> mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs