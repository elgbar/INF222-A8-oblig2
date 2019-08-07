module Syntax where

import Data.IORef
import System.IO.Unsafe
import Control.Exception
import Data.Char (isDigit)

data Ast
  = SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SDelete String
  | SArrAssign String Int Expr
  | SVarDecl String Expr
  | SVarDeclRef String [Expr] [Expr] -- todo -> done
  | SExpr Expr
  | SReturn Expr
  | STry Stmt String Stmt
  | SThrow Expr
  | SImport String
  | SAssert String Expr
  | SEof -- Skip to the end of the file


  | EVal Value
  | EVar String
  | EArrVar String Int
  | EFun [String] Stmt
  | ECall Expr [Expr] [Value]
  | ERef Expr
  | EDeref Expr
  | ESpawn Expr
  | EDetach Expr
  | EJoin Expr
  | EReset Expr
  | EShift Expr

  | Hole
  | HoleWithEnv Env

type Stmt = Ast

type Expr = Ast

type Ctx = Ast

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value) Value
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun String ([Value] -> Value)
  | VPrimFunIO String ([Value] -> IO Value)
  | VCont Env [Ctx]
  | VArr [Expr]

instance Show Ast where
  show SSkip = "SSkip"
  show (SIf e s1 s2) =
    "SIf (" ++ show e ++ ") ? " ++ show s1 ++ " : " ++ show s2
  show (SWhile e stm) = "SWhile " ++ show e ++ " " ++ show stm
  show (SBlock stm) = "SBlock " ++ show stm
  show (SSeq s1 s2) = show s1 ++ ">>" ++ show s2
  show (SAssign s e) = "SAssign " ++ s ++ "=" ++ show e
  show (SArrAssign s i e) = "SArrAssign " ++ s ++ "["++show i++"] =" ++ show e
  show (SVarDecl s e) = "SVarDecl " ++ s ++ "=" ++ show e
  show (SVarDeclRef s todo done) = "SVarDeclRef " ++ s ++ " todo " ++ show todo ++ " done "++ show done
  show (SExpr e) = "SExpr {" ++ show e ++ "}"
  show (SReturn v) = "SReturn " ++ show v
  show (STry b v c) =
    "STry {" ++ show b ++ "} (" ++ show v ++ ") {" ++ show c ++ "}"
  show (SThrow e) = "SThrow " ++ show e
  show (EVal v) = "EVal " ++ show v
  show (EVar str) = "EVar " ++ str
  show (EFun strs stm) = "EFun (" ++ show strs ++ ") {" ++ show stm ++ "}"
  show (ECall e es vs) =
    "ECall (" ++ show e ++ ") {" ++ show es ++ "} {" ++ show vs ++ "}"
  show (ERef e) = "ERef " ++ show e
  show (EDeref e) = "EDeref " ++ show e
  show Hole = "Hole"
  show (HoleWithEnv e) = "HoleWithEnv " ++ showNoPrim e
  show (SImport s) = "SImport "++show s
  show SEof = "SEof"
  show (ESpawn e) = "ESpawn(" ++ show e ++ ")"
  show (EDetach e) = "EDetach(" ++ show e ++ ")"
  show (EJoin e) = "EJoin(" ++ show e ++ ")"
  show (EReset f) = "EReset"++ show f
  show (EShift f) = "EShift"++ show f
  show (SAssert msg e) = "SAssert "++ show msg ++" "++ show e
  show (EArrVar i index) = "EArrVar "++ i ++ " " ++show index

isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _        = False
notValue = not . isValue

type Env = [(String, Value)]

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef _ t) = "ref "++val2type t
  show VVoid = "void"
  show (VClosure ss s e) =
    "closure {strs=" ++
    show ss ++ ", stmt=" ++ show s ++ ", env=" ++ showNoPrim e ++ "}"
  show (VPrimFun n _) = "prim-fun "++n
  show (VPrimFunIO n _) = "prim-fun io "++n
  show (VArr xs) = show $ map (\x -> case x of {EVal v -> v; _ -> VString $ show x}) xs

instance Eq Value where
  (==) VVoid VVoid = True
  (==) (VInt i) (VInt i') = i == i'
  (==) (VBool i) (VBool i') = i == i'
  (==) (VString i) (VString i') = i == i'
  (==) (VArr xs) (VArr ys) = unsafePerformIO $ arrEql xs ys
  (==) v1 v2 = error $ "No way to compare "++val2type v1 ++" to "++val2type v2

instance Read Value where
  readsPrec _ ('v':'o':'i':'d':inp) = return (VVoid,inp)
  readsPrec _ ('t':'r':'u':'e':inp) = return (VBool True,inp)
  readsPrec _ ('f':'a':'l':'s':'e':inp) = return (VBool False,inp)
  readsPrec _ inp = let neg = head inp == '-' -- a very shitty way of checking if this is negative
                        ds = takeWhile isDigit (if neg then tail inp else inp)
                        ds' = if neg then '-':ds else ds
                        inp' = drop (length ds') inp
                        d = (read::String->Int) ds'
                    in if length ds > 0 then return (VInt d, inp')
                       else error $ "Cannot read: "++inp

showNoPrim :: Env -> String
showNoPrim env = show $ filter (\(_, p) -> case p of 
    VPrimFunIO _ _ -> False 
    VPrimFun _ _ -> False
    _ -> True
    ) env

valName :: Env -> String
valName env = show $ map (\(n,f) -> case f of
                                      VPrimFun pn _ -> pn
                                      VPrimFunIO pn _ -> pn
                                      _ -> n
                          ) $ filter (\(n,f) -> case n of { '_':_ -> False; _ -> True}) env

startupCode :: Expr -> Ast
startupCode blk =  STry blk "__ex" (ECall (EVar "println") [EVal (VString "Uncaught exception: "), EVar "__ex"] [])

sameType :: Value -> Value -> Bool
sameType VVoid _ = True --void is equal to everything
sameType _ VVoid = True
sameType (VInt _) (VInt _) = True
sameType (VBool _) (VBool _) = True
sameType (VString _) (VString _) = True
sameType (VClosure _ _ _) (VClosure _ _ _) = True
sameType (VPrimFunIO _ _) (VPrimFunIO _ _) = True
sameType (VPrimFun _ _) (VPrimFun _ _) = True
sameType (VRef _ v1) (VRef _ v2) = sameType v1 v2
sameType (VArr _) (VArr _) = True
sameType _ _ = False

val2type :: Value -> String
val2type (VInt _)         = "integer"
val2type (VArr _)         = "array"
val2type (VBool _)        = "boolean"
val2type (VString _)      = "string"
val2type (VRef _ v)       = "ref " ++ val2type v
val2type VVoid            = "void"
val2type (VClosure _ _ _) = "closure"
val2type (VPrimFun _ _)     = "primfun"
val2type (VPrimFunIO _ _)   = "primfun io"

readRefs :: [Expr] -> [Expr]
readRefs = map (\a -> case a of {EVal (VRef nv _) -> EVal $ unsafePerformIO $ readIORef nv;_ -> a})

readRef :: Value -> IO (Maybe Value)
readRef (VRef nv ot) = do
          v' <- readIORef nv
          if sameType v' ot then return $ Just $ v'
          else error $ "Inconsistent type of derefered value. Current type: "++val2type v'++" origial type: " ++ val2type ot
readRef _ = return Nothing


writeRefs :: [Expr] -> [Expr]
writeRefs = map (\x-> unsafePerformIO $ newRef x)

newRef :: Expr -> IO Expr
newRef (ERef (EVal v)) = do
  vr <- newIORef v
  return $ EVal $ VRef vr v
newRef v = return v

writeRef :: Value -> Value -> String-> IO ()
writeRef (VRef nv ot) val _ = do
          if sameType val ot then writeIORef nv val >> return ()
          else error $ "Cannot assign "++val2type val ++ " to "++val2type ot
writeRef _ val s = error $ "Trying to assign "++val2type val++" "++show val++" to a non-ref " ++ show s

arrEql :: [Expr] -> [Expr] -> IO Bool
arrEql xs ys =
  if (length xs /= length ys) then (return False) else do
    ziped <- arrExpr2arrVal xs ys
    return $ all (\(x,y) ->  (x == y)) ziped

arrExpr2arrVal :: [Expr] -> [Expr] -> IO [(Value, Value)]
arrExpr2arrVal xs ys = do
  resx <- ecTry $ evaluate $ map expr2val xs
  resy <- ecTry $ evaluate $ map expr2val ys
  case resx of
    Right xs' -> case resy of
      Right ys' -> return $ zip xs' ys'
      Left e -> error $ "Can only compare arrays where all elements is values"
    Left e -> error $ "Can only compare arrays where all elements is values"


    
expr2val :: Expr -> Value
expr2val (EVal v) = v
expr2val e = error $ "Expression is not a value: " ++ show e

pmfTry :: IO a -> IO (Either PatternMatchFail a)
pmfTry = try

ecTry :: IO a -> IO (Either ErrorCall a)
ecTry = try