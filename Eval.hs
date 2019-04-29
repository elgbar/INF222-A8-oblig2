module Eval where

import Syntax
import Primitive
import Pretty
import Debug.Trace (trace)

import Control.Monad
import Data.IORef
import Data.Maybe

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env =
  let (Just v) = lookup s env in v -- assumes that a variable is always found

exec :: Ast -> IO ()
exec e = steps (e, primitives, [])

steps :: (Ast, Env, [Ctx]) -> IO ()
steps (SSkip, _, []) = return ()
steps st = step st >>= steps

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, SExpr Hole : ctx)
step (v, env, SExpr Hole : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, (SBlock (HoleWithEnv env)) : ctx)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, SSeq Hole s2 : ctx)
step (SSkip, env, SSeq Hole s2 : ctx) = return (s2, env, ctx)

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, SIf Hole s1 s2 : ctx)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx) = return (SBlock s1, env, ctx)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx) = return (SBlock s2, env, ctx)

step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, SVarDecl s Hole : ctx)
step (v, env, SVarDecl s Hole : ctx) | isValue v
  = return (SSkip, addVar s (expr2val v) env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, SAssign s Hole : ctx)
step (v, env, SAssign s Hole : ctx) | isValue v = do
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, ERef Hole : ctx)
step (v, env, ERef Hole : ctx) | isValue v = do
  nv <- newIORef (expr2val v)
  return (EVal (VRef nv), env, ctx)

-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, EDeref Hole : ctx)
step (v, env, EDeref Hole : ctx) | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return $ (EVal v', env, ctx)

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) = do
  return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx)
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal VVoid, env, ctx)
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx) = do
  return (EVal $ f (reverse vs), env, ctx)
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx)
step (ECall f [] _, _, _) | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function position
step (ECall f args [], env, ctx) | notValue f = return (f, env, ECall Hole args [] : ctx)
step (f, env, ECall Hole args [] : ctx) | isValue f = return (ECall f args [], env, ctx)
step (ECall f (a:args) vs, env, ctx) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx)
step (v, env, ECall f (Hole:args) vs : ctx) | isValue v = return (ECall f args (expr2val v : vs), env, ctx)

-- return
-- step (SReturn, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes
step (SReturn, env, ctx) = do
  let octx = dropWhile (\e -> isNothing (firstEnv [e])) ctx
  let oenv = case (firstEnv octx) of
                Just oenv -> oenv
                Nothing -> error ("No environment found to return to " ++ (printInfo env octx))
  return (SSkip, oenv, octx)

step (STryCatch b v c, env, ctx) = return (b, addVar v , ECall (HoleWithEnv env) [] vs: ctx) 

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (e, env, ctx) = error $ "Stuck at expression: " ++ show e ++ (printInfo env ctx)

firstEnv :: [Ctx] -> Maybe Env
firstEnv [] = Nothing
firstEnv (ECall (HoleWithEnv e) _ _:as) = Just e
firstEnv (_:as) = firstEnv as

printInfo :: Env -> [Ctx] -> String
printInfo env ctx = "\n\nEnvironment: "  ++ show (remPrimEnv env) ++ "\n\nContext: " ++ show ctx ++"\n\n"
