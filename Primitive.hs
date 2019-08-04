module Primitive where

import Control.Monad
import Syntax
import System.IO

primitives :: Env
primitives =
  [ ("__u-", VPrimFun "-" $ \[VInt x] -> VInt $ -x)
  , ("__u!", VPrimFun "!" $ \[VBool x] -> VBool $ not x)
  , ("__u++", VPrimFun "++" $ \[VInt x] -> VInt $ x + 1)
  , ("__u--", VPrimFun "--" $ \[VInt x] -> VInt $ x - 1)
  , ("__b+", VPrimFun "+" $ \[VInt x, VInt y] -> VInt $ x + y)
  , ("__b-", VPrimFun "-" $ \[VInt x, VInt y] -> VInt $ x - y)
  , ("__b*", VPrimFun "*" $ \[VInt x, VInt y] -> VInt $ x * y)
  , ("__b/", VPrimFun "/" $ \[VInt x, VInt y] -> VInt $ x `div` y)
  , ("__b%", VPrimFun "%" $ \[VInt x, VInt y] -> VInt $ x `mod` y)
  , ( "__b=="
    , VPrimFun "==" $ \args ->
        case args of
          [VBool x, VBool y] -> VBool $ x == y
          [VInt x, VInt y]   -> VBool $ x == y
          [VString x, VString y]   -> VBool $ x == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ( "__b!="
    , VPrimFun "!=" $ \args ->
        case args of
          [VBool x, VBool y] -> VBool $ not $ x == y
          [VInt x, VInt y]   -> VBool $ not $ x == y
          [VString x, VString y] -> VBool $ not $ x == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ("__b<", VPrimFun "<" $ \[VInt x, VInt y] -> VBool $ x < y)
  , ("__b<=", VPrimFun "<=" $ \[VInt x, VInt y] -> VBool $ x <= y)
  , ("__b>", VPrimFun ">" $ \[VInt x, VInt y] -> VBool $ x > y)
  , ("__b>=", VPrimFun ">="$ \[VInt x, VInt y] -> VBool $ x >= y)
  , ("__b&&", VPrimFun "&&"$ \[VBool x, VBool y] -> VBool $ x && y)
  , ("__b||", VPrimFun "||"$ \[VBool x, VBool y] -> VBool $ x || y)
  -- array functions
  , ("length", VPrimFun "length" $ \[VArr xs] -> VInt $ length xs) -- the length of an array
  , ("add", VPrimFun "add"$ \[VArr xs, e] -> VArr $ xs ++ [EVal e]) -- return a new array with the added eent
  , ("insert", VPrimFun "insert" $ \[VArr xs, e, VInt index] -> do -- insert an element at the given index
                  let (a,b) = splitAt index xs
                  VArr $ a ++ (EVal e : b))
  , ("remove", VPrimFun "remove"$ \[VArr xs, VInt index] ->  
                  if index <= 0 then  VArr $ if null xs then [] else tail xs
                  else do
                    let (a,b) = splitAt (index+1) xs
                    VArr $ safeInit a ++ b)
  , ("create", VPrimFun "create" $ \[VInt size] -> VArr $ replicate size $ ERef $ EVal VVoid) -- create an array with the given size and void as eent
  -- real IO funcs
  , ("readln", VPrimFunIO "readln" $ \[] -> getLine >>= \inp -> return $ VString inp)
  , ("print", VPrimFunIO "print" $ \args -> mapM_ (putStr . show) args >> hFlush stdout >> return VVoid)
  , ( "println"
    , VPrimFunIO "println" $ \args ->
        mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs