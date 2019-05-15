module Syntax where

import Data.IORef

data Ast
  = SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SVarDecl String Expr
  | SExpr Expr
  | SReturn Expr
  | STry Stmt String Stmt
  | SThrow Expr
  | SImport String
  | SEof -- Skip to the end of the file
  | SFor Expr Expr Expr Stmt

  | EVal Value
  | EVar String
  | EFun [String] Stmt
  | ECall Expr [Expr] [Value]
  | ERef Expr
  | EDeref Expr
  | ESpawn Expr
  | EDetach Expr
  | EJoin Expr

  | Hole
  | HoleWithEnv Env

type Stmt = Ast

type Expr = Ast

type Ctx = Ast

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value)
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)

instance Show Ast where
  show SSkip = "SSkip"
  show (SIf e s1 s2) =
    "SIf (" ++ show e ++ ")?(" ++ show s1 ++ "):(" ++ show s2 ++ ")"
  show (SWhile e stm) = "SWhile (" ++ show e ++ ") {" ++ show stm ++ "}"
  show (SBlock stm) = "SBlock {" ++ show stm ++ "}"
  show (SSeq s1 s2) = show s1 ++ " >> " ++ show s2
  show (SAssign s e) = "SAssign " ++ s ++ "=" ++ show e
  show (SVarDecl s e) = "SVarDecl " ++ s ++ "=" ++ show e
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
  show (SFor d c i s) = "EFor "++ show d++"; "++show c ++"; "++show i++"{"++show s++"}"

isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _        = False

notValue = not . isValue

expr2val :: Expr -> Value
expr2val (EVal v) = v

type Env = [(String, Value)]

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef _) = "ref"
  show VVoid = "void"
  show (VClosure ss s e) =
    "closure {strs=" ++
    show ss ++ ", stmt=" ++ show s ++ ", env=" ++ showNoPrim e ++ "}"
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"

primitiveNames =
  [ "__u-"
  , "__u!"
  , "__b+"
  , "__b-"
  , "__b*"
  , "__b/"
  , "__b%"
  , "__b=="
  , "__b!="
  , "__b<"
  , "__b<="
  , "__b>"
  , "__b>="
  , "__b&&"
  , "__b||"
  , "print"
  , "println"
  ]

showNoPrim :: Env -> String
showNoPrim env = show $ filter (\(p,_) -> p `notElem` primitiveNames) env

startupCode :: Expr -> Ast
startupCode blk =  STry blk "__ex" (ECall (EVar "println") [EVal (VString "Uncaught exception: "), EVar "__ex"] [])