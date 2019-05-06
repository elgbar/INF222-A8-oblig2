module Eval where

import Primitive
import Debug.Trace (trace)

import Data.IORef
import Data.Maybe
import System.IO.Unsafe 
import Control.Monad      (when)
import Data.Either
import Parser
import Pretty
import Syntax
import System.Environment (getArgs)

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env = fromMaybe (error $ "failed to find var '" ++ s ++ "' in env " ++ showNoPrim env) (lookup s env)

    -- content -> file -> verbose -> debug -> IO Env ->
run :: String -> String -> Bool -> Bool-> IO Env
run input fname verbose dbg = 
  case parse program fname input of
    Right v -> do
      when verbose $ putStrLn $ pPrint v
      exec v dbg
    Left e -> print e >> error "Failed to parse file"


exec :: Ast -> Bool -> IO Env
exec e d = steps (e, primitives, [], d)

steps :: (Ast, Env, [Ctx], Bool) -> IO Env
steps (SSkip, env, [], dbg) = return env
steps st = step st >>= steps

step :: (Ast, Env, [Ctx], Bool) -> IO (Ast, Env, [Ctx], Bool)
step (ast, e, c, True) | trace ("Evaluating ast: "++ show ast ++ "\n\nIn the context: " ++ show c ++"\n\nWith the Env: "++ showNoPrim e ++"\n\n\n") False = undefined

step (SImport fn, _, ctx, dbg) = do
   s <- readFile fn
   let env = unsafePerformIO $ run s fn dbg dbg -- should verbosity be passed to step?
   return (SSkip, env, ctx, dbg) --it hurts me using the unsafeIO

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx, dbg) = return (e, env, SExpr Hole : ctx, dbg)
step (v, env, SExpr Hole : ctx, dbg) | isValue v = return (SSkip, env, ctx, dbg)

-- Blocks
step (SBlock s, env, ctx, dbg) = return (s, env, SBlock (HoleWithEnv env) : ctx, dbg)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx, dbg) = return (SSkip, env, ctx, dbg) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx, dbg) = return (s1, env, SSeq Hole s2 : ctx, dbg)
step (SSkip, env, SSeq Hole s2 : ctx, dbg) = return (s2, env, ctx, dbg)

-- If and while
step (SIf cond s1 s2, env, ctx, dbg) = return (cond, env, SIf Hole s1 s2 : ctx, dbg)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx, dbg) = return (SBlock s1, env, ctx, dbg)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx, dbg) = return (SBlock s2, env, ctx, dbg)

step (w@(SWhile cond s), env, ctx, dbg) = return (SIf cond (SSeq s w) SSkip, env, ctx, dbg)

-- Variable declaration
step (SVarDecl s e, env, ctx, dbg) = return (e, env, SVarDecl s Hole : ctx, dbg)
step (v, env, SVarDecl s Hole : ctx, dbg) | isValue v
  = return (SSkip, addVar s (expr2val v) env, ctx, dbg)

-- Assignment
step (SAssign s e, env, ctx, dbg) = return (e, env, SAssign s Hole : ctx, dbg)
step (v, env, SAssign s Hole : ctx, dbg) | isValue v = 
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx, dbg)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step (EVar s, env, ctx, dbg) = return (EVal $ findVar s env, env, ctx, dbg)

-- Box a value
step (ERef e, env, ctx, dbg) = return (e, env, ERef Hole : ctx, dbg)
step (v, env, ERef Hole : ctx, dbg) | isValue v = do
  nv <- newIORef (expr2val v)
  return (EVal (VRef nv), env, ctx, dbg)

-- Dereference a ref
step (EDeref e, env, ctx, dbg) = return (e, env, EDeref Hole : ctx, dbg)
step (v, env, EDeref Hole : ctx, dbg) | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return (EVal v', env, ctx, dbg)

-- Function becomes a closure
step (EFun pars body, env, ctx, dbg) = return (EVal $ VClosure pars body env, env, ctx, dbg)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, dbg) = return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, dbg)
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, dbg) = return (EVal VVoid, env, ctx, dbg)
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx, dbg) = return (EVal $ f (reverse vs), env, ctx, dbg)
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, dbg) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx, dbg)
step (ECall f [] _, _, _,_) | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function position
step (ECall f args [], env, ctx, dbg) | notValue f = return (f, env, ECall Hole args [] : ctx, dbg)
step (f, env, ECall Hole args [] : ctx, dbg) | isValue f = return (ECall f args [], env, ctx, dbg)
step (ECall f (a:args) vs, env, ctx, dbg) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx, dbg)
step (v, env, ECall f (Hole:args) vs : ctx, dbg) | isValue v = return (ECall f args (expr2val v : vs), env, ctx, dbg)

-- return
step (SReturn Hole, env, v:ctx, dbg) | notValue v  = return (v, env, ctx, dbg) -- evaluate return expr to value (note/warn: what if never value?)
step (val@(EVal _), env, SReturn Hole : ctx, dbg) = -- must be value
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  return (val, oenv, octx, dbg)
step (SReturn val, env, ctx, dbg)  = -- initial evaluation, parse the maybe expr and put it on the stack/ctx
  return (val, env, SReturn Hole : ctx, dbg) --put what to return (val) on the stack
step (EVal VVoid, env, ctx, dbg) = return (SSkip, env,ctx,dbg) --toplevel return (hopefully)

--Throw 
step (SThrow Hole, env, v:ctx, dbg) | notValue v  = return (v, env, ctx, dbg) -- evaluate return expr to value (note/warn: what if never value?)
step (SThrow val, env, ctx, dbg) = return (val, env, SThrow Hole : ctx, dbg) --eval expr to value before catching

--Try
step (STry b v c, env, ctx, dbg) = return (b, env, STry (HoleWithEnv env) v c:ctx, dbg) -- entering a try block
--catch
 
step (EVal v, env, SThrow Hole : ctx, dbg) = -- must be value
  let ((s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  return (blk, addVar s v env, octx, dbg)
step (SSkip, env, STry (HoleWithEnv e) _ _:ctx, dbg) = return (SSkip, e, ctx, dbg) -- nothing was thrown


step (SEof, env, ctx, dbg) = return (SSkip, env, [], dbg)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (e, env, ctx, dbg) = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx

escapeHole :: Env -> [Ctx] -> (Ctx -> Maybe a) -> (a, [Ctx])
escapeHole env ctx f = do
                let octx = dropWhile (isNothing . f) ctx
                let ret = fromMaybe (error "Failed to escape hole") (f (head octx))
                (ret, if null octx then [] else tail octx)

              
firstCall :: Ctx -> Maybe Env
firstCall (ECall (HoleWithEnv e) _ _) = Just e
firstCall _ = Nothing

firstTry :: Ctx -> Maybe (String, Stmt)
firstTry (STry _ s cb) = Just (s, cb)
firstTry _ = Nothing 

printInfo :: Env -> [Ctx] -> String
printInfo env ctx = "\n\nEnvironment: "  ++ showNoPrim env ++ "\n\nContext: " ++ show ctx ++"\n\n"
