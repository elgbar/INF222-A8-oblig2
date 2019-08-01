module Primitive where

import Control.Monad
import Syntax
import System.IO

primitives :: Env
primitives =
  [ ("__u-", VPrimFun $ \[VInt x] -> VInt $ -x)
  , ("__u!", VPrimFun $ \[VBool x] -> VBool $ not x)
  , ("__u||", VPrimFun $ \[VBool x, VBool y] -> VBool $ x || y)
  , ("__b+", VPrimFun $ \[VInt x, VInt y] -> VInt $ x + y)
  , ("__b-", VPrimFun $ \[VInt x, VInt y] -> VInt $ x - y)
  , ("__b*", VPrimFun $ \[VInt x, VInt y] -> VInt $ x * y)
  , ("__b/", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `div` y)
  , ("__b%", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `mod` y)
  , ( "__b=="
    , VPrimFun $ \args ->
        case args of
          [VBool x, VBool y] -> VBool $ x == y
          [VInt x, VInt y]   -> VBool $ x == y
          [VString x, VString y]   -> VBool $ x == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ( "__b!="
    , VPrimFun $ \args ->
        case args of
          [VBool x, VBool y] -> VBool $ not $ x == y
          [VInt x, VInt y]   -> VBool $ not $ x == y
          [VString x, VString y] -> VBool $ not $ x == y
          [v1, v2] -> error $ "No way to compare "++val2type v1 ++" to "++val2type v2)
  , ("__b<", VPrimFun $ \[VInt x, VInt y] -> VBool $ x < y)
  , ("__b<=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x <= y)
  , ("__b>", VPrimFun $ \[VInt x, VInt y] -> VBool $ x > y)
  , ("__b>=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x >= y)
  , ("__b&&", VPrimFun $ \[VBool x, VBool y] -> VBool $ x && y)
  , ("__b||", VPrimFun $ \[VBool x, VBool y] -> VBool $ x || y)
  , ("readln", VPrimFunIO $ \[] -> getLine >>= \inp -> return $ VString inp)
  , ("print", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> hFlush stdout >> return VVoid)
  , ( "println"
    , VPrimFunIO $ \args ->
        mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]