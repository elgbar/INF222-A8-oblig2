module Syntax where

import Data.IORef

data Ast
  = SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SArrAssign String Int Expr
  | SVarDecl String Expr
  | SVarDeclRef String [Expr] [Expr] -- todo -> done
  | SExpr Expr
  | SReturn Expr
  | STry Stmt String Stmt
  | SThrow Expr
  | SImport String
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
  | SAssert String Expr

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
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)
  | VCont Env [Ctx]
  | VArr [Expr]

instance Show Ast where
  show SSkip = "SSkip"
  show (SIf e s1 s2) =
    "SIf (" ++ show e ++ ")?(" ++ show s1 ++ "):(" ++ show s2 ++ ")"
  show (SWhile e stm) = "SWhile " ++ show e ++ " " ++ show stm
  show (SBlock stm) = "SBlock " ++ show stm
  show (SSeq s1 s2) = show s1 ++ "\n" ++ show s2 ++ "\n"
  show (SAssign s e) = "SAssign " ++ s ++ "=" ++ show e
  show (SArrAssign s i e) = "SArrAssign " ++ s ++ "["++show i++"] =" ++ show e
  show (SVarDecl s e) = "SVarDecl " ++ s ++ "=" ++ show e
  show (SVarDeclRef s todo done) = "SVarDeclRef " ++ s ++ " todo " ++ show todo ++ " done "++ show done
  show (SExpr e) = "SExpr {" ++ show e ++ "}"
  show (SReturn v) = "SReturn " ++ show v
  show (STry b v c) =
    "STry {\n" ++ show b ++ "\n} (" ++ show v ++ ") {" ++ show c ++ "}"
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

expr2val :: Expr -> Value
expr2val (EVal v) = v
expr2val e = error $ "Expression is not a value: " ++ show e

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
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"
  show (VArr vals) = show vals

showNoPrim :: Env -> String
showNoPrim env = show $ filter (\(_, p) -> case p of 
    VPrimFunIO _ -> False 
    VPrimFun _ -> False
    _ -> True
    ) env

valName :: Env -> String
valName env = show $ map fst $ filter (\(_,f) -> case f of; VPrimFun _ -> False; _ -> True) env

startupCode :: Expr -> Ast
startupCode blk =  STry blk "__ex" (ECall (EVar "println") [EVal (VString "Uncaught exception: "), EVar "__ex"] [])


sameType :: Value -> Value -> Bool
sameType VVoid VVoid = True
sameType (VInt _) (VInt _) = True
sameType (VBool _) (VBool _) = True
sameType (VString _) (VString _) = True
sameType (VClosure _ _ _) (VClosure _ _ _) = True
sameType (VPrimFunIO _) (VPrimFunIO _) = True
sameType (VPrimFun _) (VPrimFun _) = True
sameType (VRef _ v1) (VRef _ v2) = sameType v1 v2
sameType (VArr _) (VArr _) = True
sameType _ _ = False

val2type :: Value -> String
val2type (VInt _)         = "integer"
val2type (VArr vals)      = "array"
val2type (VBool _)        = "boolean"
val2type (VString _)      = "string"
val2type (VRef _ v)       = "ref " ++ val2type v
val2type VVoid            = "void"
val2type (VClosure _ _ _) = "closure"
val2type (VPrimFun _)     = "primfun"
val2type (VPrimFunIO _)   = "primfun io"