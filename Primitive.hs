module Primitive where

import Control.Monad
import Syntax
import System.IO

primitives :: Env
primitives =
  [ ("__u-", VPrimFun $ \[VInt xs] -> VInt $ -xs)
  , ("__u!", VPrimFun $ \[VBool xs] -> VBool $ not xs)
  , ("__u||", VPrimFun $ \[VBool xs, VBool y] -> VBool $ xs || y)
  , ("__b+", VPrimFun $ \[VInt xs, VInt y] -> VInt $ xs + y)
  , ("__b-", VPrimFun $ \[VInt xs, VInt y] -> VInt $ xs - y)
  , ("__b*", VPrimFun $ \[VInt xs, VInt y] -> VInt $ xs * y)
  , ("__b/", VPrimFun $ \[VInt xs, VInt y] -> VInt $ xs `div` y)
  , ("__b%", VPrimFun $ \[VInt xs, VInt y] -> VInt $ xs `mod` y)
  , ( "__b=="
    , VPrimFun $ \args ->
        case args of
          [VBool xs, VBool y] -> VBool $ xs == y
          [VInt xs, VInt y]   -> VBool $ xs == y
          [VString xs, VString y]   -> VBool $ xs == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ( "__b!="
    , VPrimFun $ \args ->
        case args of
          [VBool xs, VBool y] -> VBool $ not $ xs == y
          [VInt xs, VInt y]   -> VBool $ not $ xs == y
          [VString xs, VString y] -> VBool $ not $ xs == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ("__b<", VPrimFun $ \[VInt xs, VInt y] -> VBool $ xs < y)
  , ("__b<=", VPrimFun $ \[VInt xs, VInt y] -> VBool $ xs <= y)
  , ("__b>", VPrimFun $ \[VInt xs, VInt y] -> VBool $ xs > y)
  , ("__b>=", VPrimFun $ \[VInt xs, VInt y] -> VBool $ xs >= y)
  , ("__b&&", VPrimFun $ \[VBool xs, VBool y] -> VBool $ xs && y)
  , ("__b||", VPrimFun $ \[VBool xs, VBool y] -> VBool $ xs || y)
  -- array functions
  , ("length", VPrimFunIO $ \[VArr xs] -> return $ VInt $ length xs) -- the length of an array
  , ("add", VPrimFunIO $ \[VArr xs, e] -> return $ VArr $ xs ++ [ERef $ EVal e]) -- return a new array with the added eent
  , ("insert", VPrimFunIO $ \[VArr xs, e, VInt index] -> do -- insert an element at the given index
                  let (a,b) = splitAt index xs
                  return $ VArr $ a ++ (EVal e : b))
  , ("remove", VPrimFunIO $ \[VArr xs, VInt index] ->  
                  if index <= 0 then return $ VArr $ if null xs then [] else tail xs
                  else do
                    let (a,b) = splitAt (index+1) xs
                    return $ VArr $ safeInit a ++ b)
  , ("create", VPrimFunIO $ \[VInt size] -> return $ VArr $ replicate size $ ERef $ EVal VVoid) -- create an array with the given size and void as eent
  -- real IO funcs
  , ("readln", VPrimFunIO $ \[] -> getLine >>= \inp -> return $ VString inp)
  , ("print", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> hFlush stdout >> return VVoid)
  , ( "println"
    , VPrimFunIO $ \args ->
        mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs