module Eval where

import Primitive
import Debug.Trace (trace)

import Data.IORef
import Data.Maybe
import System.IO.Unsafe
import Control.Monad
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
run :: String -> String -> Bool -> Bool -> IO Env
run input fname verbose dbg  =
  case parse program fname input of
    Right v -> do
      when verbose $ putStrLn $ pPrint v
      exec v dbg
    Left e -> print e >> error "Failed to parse file"

type Thread = (Ast, Env, [Ctx], Int, Int)

getNextId :: [Thread] -> Int
getNextId threads = foldl max 0 (map threadId threads) + 1

threadExists :: Int -> [Thread] -> Bool
threadExists tid threads = tid `elem` map threadId threads

threadId :: Thread -> Int
threadId (_, _, _, tid, _) = tid

parentId :: Thread -> Int
parentId (_, _, _, _, ptid) = ptid

getThread :: Int -> [Thread] -> Thread
getThread tid threads =
  case filter (\t -> tid == threadId t) threads of
    [t] -> t
    _ -> error $ "Failed to find a thread with the id " ++ show tid

eqThread :: Thread -> Thread -> Bool
eqThread t1 t2 = threadId t1 == threadId t2
notEqThread t1 t2 = not $ eqThread t1 t2

exec :: Ast -> Bool -> IO Env
exec e = steps [(e, primitives, [], 0, 0)]

      -- threads to run -> threads ran, _ _ -> new state
steps :: [Thread] -> Bool -> IO Env
--fmap concat (
steps thrds dbg  = do
    threads <- stepN thrds dbg 4 -- step in each thread
    let (_, mainEnv, _, _, _) = getThread 0 threads
    running <- filterM (\f -> case f of {(SSkip, _, [], n, _) -> return False ; otherwise -> return True}) threads -- filter out ended threads
    alive <- filterM (\f -> return $ threadExists (parentId f) running) running -- filter out threads where the parent has stopped
    case alive of
      _ | dbg, trace ("\nThreads alive: "++show (length alive)) False -> undefined
      [] -> return mainEnv
      _ -> steps alive dbg

-- Step N times
stepN :: [Thread] -> Bool -> Int -> IO [Thread]
stepN threads dbg n = do
  trds@(t:ts) <- step threads dbg
  case t of
    -- debug tracing
    _ | dbg, trace ("\nStep: "++show n) False -> undefined
     --nothing more to evaluate, main thread is still needed
    (SSkip, _, [], _, _) -> return [getThread 0 trds]
    -- When a thread is waiting for another thread make sure it uses as little resources as possible
    -- Note this is fair as nothing will happen to the other threads while this (waiting) thread is running
    (_, _, EJoin Hole : _, _, _) -> return $ ts ++ [t]
    _ ->  case n of
        0 -> return $ ts ++ [t]
        n -> stepN trds dbg (n-1)

step :: [Thread] -> Bool -> IO [Thread]
step ((ast, e, c, tid, ptid) : ts) True | trace ("Evaluating (tid "++show tid++", pid "++show ptid++")\nast: "++ show ast ++ "\n\nctx: " ++ show c ++"\n\nenv: "++ showNoPrim e ++"\n\n\n") False = undefined

-- step fr@(SSkip, _, [], _, _) : ts) _ = return [fr]

-- Statement expression: evaluate expression and turn into SSkip
step ((SExpr e, env, ctx, tid, ptid) : ts) _ = return ((e, env, SExpr Hole : ctx, tid, ptid):ts)
step ((v, env, SExpr Hole : ctx, tid, ptid) : ts) _ | isValue v = return ((SSkip, env, ctx, tid, ptid):ts)

-- Blocks
step ((SBlock s, env, ctx, tid, ptid) : ts) _ = return ((s, env, SBlock (HoleWithEnv env) : ctx, tid, ptid):ts)
step ((SSkip, _, SBlock (HoleWithEnv env) : ctx, tid, ptid) : ts) _ = return ((SSkip, env, ctx, tid, ptid):ts) -- restore environment when block closes

-- Sequences
step ((SSeq s1 s2, env, ctx, tid, ptid) : ts) _ = return ((s1, env, SSeq Hole s2 : ctx, tid, ptid):ts)
step ((SSkip, env, SSeq Hole s2 : ctx, tid, ptid) : ts) _ = return ((s2, env, ctx, tid, ptid):ts)

-- If and while and for
step ((SIf cond s1 s2, env, ctx, tid, ptid) : ts) _ = return ((cond, env, SIf Hole s1 s2 : ctx, tid, ptid):ts)
step ((EVal (VBool True), env, SIf Hole s1 _ : ctx, tid, ptid) : ts) _ = return ((SBlock s1, env, ctx, tid, ptid):ts)
step ((EVal (VBool False), env, SIf Hole _ s2 : ctx, tid, ptid) : ts) _ = return ((SBlock s2, env, ctx, tid, ptid):ts)

step ((w@(SWhile cond s), env, ctx, tid, ptid) : ts) _ = return ((SIf cond (SSeq s w) SSkip, env, ctx, tid, ptid):ts)

-- declare the var, then create a while loop where the statement is the given statment plus the incrementor
step ((SFor dec cond inc s, env,ctx,tid,ptid):ts) _ = return ((SBlock (SSeq dec (SWhile cond (SSeq s inc))),env,ctx,tid,ptid):ts)

-- Variable declaration
step ((SVarDecl s e, env, ctx, tid, ptid) : ts) _ = return ((e, env, SVarDecl s Hole : ctx, tid, ptid):ts)
step ((v, env, SVarDecl s Hole : ctx, tid, ptid) : ts) _ | isValue v
  = return ((SSkip, addVar s (expr2val v) env, ctx, tid, ptid):ts)

-- Assignment
step ((SAssign s e, env, ctx, tid, ptid) : ts) _ = return ((e, env, SAssign s Hole : ctx, tid, ptid):ts)
step ((v, env, SAssign s Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = expr2val v
  case findVar s env of
    (VRef nv ov) -> do
      if sameType val ov then writeIORef nv val >> return ((SSkip, env, ctx, tid, ptid):ts)
      else ioError $ userError $ "Cannot assign "++val2type val ++ " to "++val2type ov
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step ((EVar s, env, ctx, tid, ptid) : ts) _ = return ((EVal $ findVar s env, env, ctx, tid, ptid):ts)

-- Box a value
step ((ERef e, env, ctx, tid, ptid) : ts) _ = return ((e, env, ERef Hole : ctx, tid, ptid):ts)
step ((v, env, ERef Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = expr2val v
  nv <- newIORef val
  return ((EVal (VRef nv val), env, ctx, tid, ptid):ts)

-- Dereference a ref
step ((EDeref e, env, ctx, tid, ptid) : ts) _ = return ((e, env, EDeref Hole : ctx, tid, ptid):ts)
step ((v, env, EDeref Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let (VRef nv ov) = expr2val v
  v' <- readIORef nv
  if sameType v' ov then return ((EVal v', env, ctx, tid, ptid):ts)
  else error $ "Inconsistent type of derefered value. Curr val: "++val2type v'++" origial val: " ++ val2type ov

-- Function becomes a closure
step ((EFun pars body, env, ctx, tid, ptid) : ts) _ = return ((EVal $ VClosure pars body env, env, ctx, tid, ptid):ts)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step ((ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, tid, ptid) : ts) _ = return ((s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, tid, ptid):ts)
step ((SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, tid, ptid) : ts) _ = return ((EVal VVoid, env, ctx, tid, ptid):ts)
  -- function body fully evaluated, return VVoid

step ((ECall (EVal (VPrimFun f)) [] vs, env, ctx, tid, ptid) : ts) _ = return ((EVal $ f (reverse vs), env, ctx, tid, ptid):ts)
step ((ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, tid, ptid) : ts) _ = do
  res  <- f (reverse vs)
  return ((EVal res, env, ctx, tid, ptid):ts)
step ((ECall f [] _, _, _, _, _) : ts) _ | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function posi tion
step ((ECall f args [], env, ctx, tid, ptid) : ts) _ | notValue f = return ((f, env, ECall Hole args [] : ctx, tid, ptid):ts)
step ((f, env, ECall Hole args [] : ctx, tid, ptid) : ts) _ | isValue f = return ((ECall f args [], env, ctx, tid, ptid):ts)
step ((ECall f (a:args) vs, env, ctx, tid, ptid) : ts) _ | isValue f = return ((a, env, ECall f (Hole:args) vs : ctx, tid, ptid):ts)
step ((v, env, ECall f (Hole:args) vs : ctx, tid, ptid) : ts) _ | isValue v = return ((ECall f args (expr2val v : vs), env, ctx, tid, ptid):ts)

-- return
step ((SReturn Hole, env, v:ctx, tid, ptid) : ts) _ | notValue v  = return ((v, env, ctx, tid, ptid):ts) -- evaluate return expr to value (note/warn: what if never value?)
step ((val@(EVal _), env, SReturn Hole : ctx, tid, ptid) : ts) _ = -- must be value
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  return ((val, oenv, octx, tid, ptid):ts)
step ((SReturn val, env, ctx, tid, ptid) : ts) _ =  -- initial evaluation, parse the maybe expr and put it on the stack/ctx
    return ((val, env, SReturn Hole : ctx, tid, ptid):ts) --put what to return (val) on the stack
step ((EVal VVoid, env, ctx, tid, ptid) : ts) _ = return ((SSkip, env, ctx, tid, ptid):ts) --toplevel return ((hopefully)

  -- Throw
step ((SThrow Hole, env, v:ctx, tid, ptid) : ts) _ | notValue v  = return ((v, env, ctx, tid, ptid):ts) -- evaluate return expr to value (note/warn: what if never value?)
step ((SThrow val, env, ctx, tid, ptid) : ts) _ = return ((val, env, SThrow Hole : ctx, tid, ptid):ts) --eval expr to value before catching

--Try
step ((STry b v c, env, ctx, tid, ptid) : ts) _ = return ((b, env, STry (HoleWithEnv env) v c:ctx, tid, ptid):ts) -- entering a try block
--catch
step ((EVal v, env, SThrow Hole : ctx, tid, ptid) : ts) _ = -- must be value
  let ((e, s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  return ((blk, addVar s v e, SBlock (HoleWithEnv e) : octx, tid, ptid):ts)
step ((SSkip, env, STry (HoleWithEnv e) _ _ : ctx, tid, ptid):ts) _ = return ((SSkip, e, ctx, tid, ptid):ts) -- nothing was thrown

-- import
step ((SImport fn, oldEnv, ctx, tid, ptid):ts) dbg = do
   s <- readFile fn
   let env = unsafePerformIO $ run s fn dbg dbg -- should verbosity be passed to step?
   return ((SSkip, oldEnv ++ env, ctx, tid, ptid):ts) --it hurts me using the unsafeIO
step ((SEof, env, ctx, tid, ptid) : ts) _ = return ((SSkip, env, [], tid, ptid):ts) -- reached end of file, its here to pass env after an import

step threads@((ESpawn e, env, ctx, tid, ptid):ts) _  = do
  let ntid = getNextId threads
  return ((EVal (VInt ntid), env, ctx, tid, ptid): (startupCode e, env, [], ntid, tid) : ts)

step ((EDetach e, env, ctx, tid, ptid) : ts) _ | notValue e = return ((e, env, EDetach Hole : ctx, tid, ptid):ts) -- parse expr
step threads@((EVal (VInt n), env, EDetach Hole : ctx, tid, ptid):ts) _ = do
      let th@(a,e,c,t,p) = getThread n threads
      return ((EVal VVoid, env, ctx, tid, 0) : (a,e,c,t,0): filter (eqThread th) threads)
step ((EVal v, env, EDetach Hole : ctx, tid, ptid) : ts) _ = error "Thread ids can only be integers"

step ((EJoin e, env, ctx, tid, ptid) : ts) _ | notValue e = return ((e, env, EJoin Hole : ctx, tid, ptid):ts) -- parse expr
step threads@(f@(EVal (VInt n), env, EJoin Hole : ctx, tid, ptid):ts) _ | threadExists n threads = return (f:ts) -- given thread id still exists so we continue to wait
step threads@((EVal (VInt n), env, EJoin Hole : ctx, tid, ptid):ts) _ | not $ threadExists n threads = return ((EVal VVoid, env, ctx, tid, ptid):ts) --return as a void as this it is wrapped in an SExpr
step ((EVal v, env, EJoin Hole : ctx, tid, ptid) : ts) _ = error "Thread ids can only be integers"


 --if there are nothing more to parse ignore the return value as it cannot be used anyway
step ((val, env, [], tid, ptid):ts) _| isValue val = return ((SSkip,env, [], tid, ptid):ts)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step ((e, env, ctx, tid, ptid) : ts) _ = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx

escapeHole :: Env -> [Ctx] -> (Ctx -> Maybe a) -> (a, [Ctx])
escapeHole env ctx f = do
                let octx = dropWhile (isNothing . f) ctx
                let ret = case octx of
                      [] -> error "No context left"
                      (oc:_) -> fromMaybe (error "Failed to escape hole") (f oc)
                (ret, if null octx then [] else tail octx)


firstCall :: Ctx -> Maybe Env
firstCall (ECall (HoleWithEnv e) _ _) = Just e
firstCall _ = Nothing

firstTry :: Ctx -> Maybe (Env, String, Stmt)
firstTry (STry (HoleWithEnv e) s cb) = Just (e,s, cb)
firstTry _ = Nothing

printInfo :: Env -> [Ctx] -> String
printInfo env ctx =  "\n\nContext: " ++ show ctx ++"\n\nEnvironment: "  ++ showNoPrim env
