module Eval where

import Primitive
import Debug.Trace (trace)

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Exception
import Data.Either
import Parser
import Pretty
import Syntax
import System.Environment (getArgs)
import System.IO

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env = fromMaybe (error $ "failed to find var '" ++ s ++ "' in env " ++ valName env) (lookup s env)

    -- content -> file -> verbose -> debug -> print assembly -> IO Env
run :: String -> String -> Env -> Bool -> Bool -> Bool -> IO Env
run input fname env verbose dbg code = 
  case parse program fname input of
    Right v -> do
      when verbose $ putStrLn $ pPrint v
      when code $ print v
      exec v env dbg
      -- case try () of
      --   Right env' -> env'
      --   Left e -> do
      --     putStrLn "Failed to evaluate expression"
      --     print e
      --     evaluate env
        
    Left e -> do
      putStrLn "Failed to parse file"
      print e
      evaluate env

runInterp :: Env -> Bool -> Bool -> Bool -> IO Env
runInterp env verbose dbg code = do
  putStr "> "
  hFlush stdout -- force flushing of text
  line <- getLine
  ran <- (try :: IO a -> IO (Either ErrorCall a)) $ run line "<console>" env verbose dbg code 
  env' <- case ran of
    Right env' -> return env' --runInterp env' verbose dbg code
    Left err -> print err >> return env --if isEOFError e then return() else ioError e
  runInterp env' verbose dbg code

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

exec :: Ast -> Env -> Bool -> IO Env
exec e [] = exec e primitives
exec e env = steps [(e, env, [], 0, 0)]

steps :: [Thread] -> Bool -> IO Env
steps thrds dbg  = do
    threads <- stepN thrds dbg 4 -- step in each thread
    -- mapM_ putStrLn ["tid: "++show tid++" ptid: "++ show ptid | (_,_,_,tid, ptid) <- threads]
    -- putStrLn("")
    let (_, mainEnv, _, _, _) = getThread 0 threads
    running <- filterM (\f -> case f of {(SSkip, _, [], n, _) -> evaluate False ; _ -> evaluate True}) threads -- filter out ended threads
    -- let _ = trace "" False
    alive <- filterM (\f -> evaluate $ threadExists (parentId f) running) running -- filter out threads where the parent has stopped
    case alive of
      _ | dbg, trace ("\nThreads alive: "++show (length alive)) False -> undefined
      [] -> evaluate mainEnv
      _ -> steps alive dbg

-- Step N times
stepN :: [Thread] -> Bool -> Int -> IO [Thread]
stepN threads dbg n = do
  trds@(t:ts) <- step threads dbg
  case t of
    _ | dbg, trace ("\nStep: "++show n) False -> undefined
    -- Nothing more to evaluate, main thread is still needed
    (SSkip, _, [], _, _) -> if (threadId t == 0) then evaluate [t] else evaluate ts
    -- When a thread is waiting for another thread make sure it uses as little resources as possible
    -- Note this is fair as nothing will happen to the other threads while this (waiting) thread is running
    (_, _, EJoin Hole : _, _, _) -> evaluate $ ts ++ [t]
    _ ->  case n of
        0 -> evaluate $ ts ++ [t] -- Move the running thread to the back of the queue
        n -> stepN trds dbg (n-1)

step :: [Thread] -> Bool -> IO [Thread]
step ((ast, e, c, tid, ptid) : ts) True | trace ("Evaluating (tid "++show tid++", pid "++show ptid++")\nast: "++ show ast ++ "\n\nctx: " ++ show c ++"\n\nenv: "++ showNoPrim e ++"\n\n\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step ((SExpr e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, SExpr Hole : ctx, tid, ptid):ts)
step ((v, env, SExpr Hole : ctx, tid, ptid) : ts) _ | isValue v = evaluate ((SSkip, env, ctx, tid, ptid):ts)

-- Blocks
step ((SBlock s, env, ctx, tid, ptid) : ts) _ = evaluate ((s, env, SBlock (HoleWithEnv env) : ctx, tid, ptid):ts)
step ((SSkip, _, SBlock (HoleWithEnv env) : ctx, tid, ptid) : ts) _ = evaluate ((SSkip, env, ctx, tid, ptid):ts) -- restore environment when block closes

-- Sequences
step ((SSeq s1 s2, env, ctx, tid, ptid) : ts) _ = evaluate ((s1, env, SSeq Hole s2 : ctx, tid, ptid):ts)
step ((SSkip, env, SSeq Hole s2 : ctx, tid, ptid) : ts) _ = evaluate ((s2, env, ctx, tid, ptid):ts)

-- If and while and for
step ((SIf cond s1 s2, env, ctx, tid, ptid) : ts) _ = evaluate ((cond, env, SIf Hole s1 s2 : ctx, tid, ptid):ts)
step ((EVal (VBool True), env, SIf Hole s1 _ : ctx, tid, ptid) : ts) _ = evaluate ((SBlock s1, env, ctx, tid, ptid):ts)
step ((EVal (VBool False), env, SIf Hole _ s2 : ctx, tid, ptid) : ts) _ = evaluate ((SBlock s2, env, ctx, tid, ptid):ts)

step ((w@(SWhile cond s), env, ctx, tid, ptid) : ts) _ = evaluate ((SIf cond (SSeq s w) SSkip, env, ctx, tid, ptid):ts)

-- Variable declaration
step ((SVarDecl s e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, SVarDecl s Hole : ctx, tid, ptid):ts)
step ((v, env, SVarDecl s Hole : ctx, tid, ptid) : ts) _ | isValue v
  = evaluate ((SSkip, addVar s (expr2val v) env, ctx, tid, ptid):ts)

-- Assignment
step ((SAssign s e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, SAssign s Hole : ctx, tid, ptid):ts)
step ((v, env, SAssign s Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = expr2val v
  case findVar s env of
    (VRef nv ov) ->
      if sameType val ov then writeIORef nv val >> evaluate ((SSkip, env, ctx, tid, ptid):ts)
      else error $ "Cannot assign "++val2type val ++ " to "++val2type ov
    _ -> error $ "Trying to assign to a non-ref \"" ++ s ++ "\""

-- Variable reference: get from environment
step ((EVar s, env, ctx, tid, ptid) : ts) _ = evaluate ((EVal $ findVar s env, env, ctx, tid, ptid):ts)
step ((EArrVar s i, env, ctx, tid, ptid) : ts) _ = do
  let (VArr meat) = findVar s env
  let expr = meat !! i
  evaluate ((expr, env, ctx, tid, ptid):ts)

-- Box a value
step ((ERef e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, ERef Hole : ctx, tid, ptid):ts)
step ((v, env, ERef Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = expr2val v
  nv <- newIORef val
  evaluate ((EVal (VRef nv val), env, ctx, tid, ptid):ts)

-- Dereference a ref
step ((EDeref e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, EDeref Hole : ctx, tid, ptid):ts)
step ((v, env, EDeref Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let (VRef nv ov) = expr2val v
  v' <- readIORef nv
  if sameType v' ov then evaluate ((EVal v', env, ctx, tid, ptid):ts)
  else error $ "Inconsistent type of derefered value. Curr val: "++val2type v'++" origial val: " ++ val2type ov

-- Function becomes a closure
step ((EFun pars body, env, ctx, tid, ptid) : ts) _ = evaluate ((EVal $ VClosure pars body env, env, ctx, tid, ptid):ts)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step ((ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, tid, ptid) : ts) _ = evaluate ((s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, tid, ptid):ts)
step ((SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, tid, ptid) : ts) _ = evaluate ((EVal VVoid, env, ctx, tid, ptid):ts)
  -- function body fully evaluated, evaluate VVoid

step ((ECall (EVal (VPrimFun f)) [] vs, env, ctx, tid, ptid) : ts) _ = evaluate ((EVal $ f (reverse vs), env, ctx, tid, ptid):ts)
step ((ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, tid, ptid) : ts) _ = do
  res  <- f (reverse vs)
  evaluate ((EVal res, env, ctx, tid, ptid):ts)
step ((ECall f [] _, _, _, _, _) : ts) _ | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function position
step ((ECall f args [], env, ctx, tid, ptid) : ts) _ | notValue f = evaluate ((f, env, ECall Hole args [] : ctx, tid, ptid):ts)
step ((f, env, ECall Hole args [] : ctx, tid, ptid) : ts) _ | isValue f = evaluate ((ECall f args [], env, ctx, tid, ptid):ts)
step ((ECall f (a:args) vs, env, ctx, tid, ptid) : ts) _ | isValue f = evaluate ((a, env, ECall f (Hole:args) vs : ctx, tid, ptid):ts)
step ((v, env, ECall f (Hole:args) vs : ctx, tid, ptid) : ts) _ | isValue v = evaluate ((ECall f args (expr2val v : vs), env, ctx, tid, ptid):ts)

-- evaluate
step ((SReturn Hole, env, v:ctx, tid, ptid) : ts) _ | notValue v  = evaluate ((v, env, ctx, tid, ptid):ts) -- evaluate evaluate expr to value (note/warn: what if never value?)
step ((val, env, SReturn Hole : ctx, tid, ptid) : ts) _ | isValue val=
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  evaluate ((val, oenv, octx, tid, ptid):ts)
step ((SReturn val, env, ctx, tid, ptid) : ts) _ = evaluate ((val, env, SReturn Hole : ctx, tid, ptid):ts)
step ((EVal VVoid, env, ctx, tid, ptid) : ts) _ = evaluate ((SSkip, env, ctx, tid, ptid):ts) --toplevel evaluate

-- Throw
step ((SThrow Hole, env, v:ctx, tid, ptid) : ts) _ | notValue v  = evaluate ((v, env, ctx, tid, ptid):ts) -- evaluate evaluate expr to value (note/warn: what if never value?)
step ((SThrow val, env, ctx, tid, ptid) : ts) _ = evaluate ((val, env, SThrow Hole : ctx, tid, ptid):ts) --eval expr to value before catching

--Try
step ((STry b v c, env, ctx, tid, ptid) : ts) _ = evaluate ((b, env, STry (HoleWithEnv env) v c:ctx, tid, ptid):ts) -- entering a try block
--catch
step ((EVal v, env, SThrow Hole : ctx, tid, ptid) : ts) _ = -- must be value
  let ((e, s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  evaluate ((blk, addVar s v e, SBlock (HoleWithEnv e) : octx, tid, ptid):ts)
step ((SSkip, env, STry (HoleWithEnv e) _ _ : ctx, tid, ptid):ts) _ = evaluate ((SSkip, e, ctx, tid, ptid):ts) -- nothing was thrown

-- import
step ((SImport fn, oldEnv, ctx, tid, ptid):ts) dbg = do
   s <- readFile fn
   env <- run s fn [] dbg dbg False -- should verbosity be passed to step?
   evaluate ((SSkip, oldEnv ++ env, ctx, tid, ptid):ts)
step ((SEof, env, ctx, tid, ptid) : ts) _ = evaluate ((SSkip, env, [], tid, ptid):ts) -- reached end of file, its here to pass env after an import

step threads@((ESpawn e, env, ctx, tid, ptid):ts) _  = do
  let ntid = getNextId threads
  evaluate ((EVal (VInt ntid), env, ctx, tid, ptid) : (startupCode e, env, [], ntid, tid) : ts)

step ((EDetach e, env, ctx, tid, ptid) : ts) _ | notValue e = evaluate ((e, env, EDetach Hole : ctx, tid, ptid):ts) -- parse expr
step threads@((EVal (VInt n), env, EDetach Hole : ctx, tid, ptid):ts) _ = do
      let (a,e,c,t,p) = getThread n threads
      evaluate ((EVal VVoid, env, ctx, tid, ptid) : (a,e,c,t,0) : filter (\tr -> threadId tr /= t) ts)
step ((EVal v, env, EDetach Hole : ctx, tid, ptid) : ts) _ = error "Thread ids can only be integers"

step ((EJoin e, env, ctx, tid, ptid) : ts) _ | notValue e = evaluate ((e, env, EJoin Hole : ctx, tid, ptid):ts) -- parse expr
step threads@(f@(EVal (VInt n), env, EJoin Hole : ctx, tid, ptid):ts) _ | threadExists n threads = evaluate (f:ts) -- given thread id still exists so we continue to wait
step threads@((EVal (VInt n), env, EJoin Hole : ctx, tid, ptid):ts) _ | not $ threadExists n threads = evaluate ((EVal VVoid, env, ctx, tid, ptid):ts) --evaluate as a void as this it is wrapped in an SExpr
step ((EVal v, env, EJoin Hole : ctx, tid, ptid) : ts) _ = error "Thread ids can only be integers"

step ((EReset f, env, ctx, tid, ptid) : ts) _ = evaluate $ (f, env, EReset Hole : ctx, tid, ptid) : ts --evaluate to closure
step ((SSkip, env, EReset Hole : ctx, tid, ptid) : ts) _ = evaluate $ (EVal VVoid, env, ctx, tid, ptid) : ts --No shift found
step ((v@(EVal (VClosure _ _ _)), env, EReset Hole : ctx, tid, ptid) : ts) _ =
     evaluate $ (ECall v [] [], env, EReset Hole : ctx, tid, ptid) : ts

step ((EShift f, env, ctx, tid, ptid) : ts) _  = do
  let nctx = takeWhile notReset ctx
  evaluate $ (f, env, EVal (VCont env nctx) : ctx, tid, ptid) : ts
step ((v@(EVal (VClosure _ _ _)), env, EVal (VCont nenv nctx) : ctx, tid, ptid) : ts) _ = 
  evaluate $ (ECall v [] [], nenv, nctx, tid, ptid) : ts

step ((SAssert msg e, env, ctx, tid, ptid):ts) _ = evaluate ((e, env, SAssert msg Hole : ctx, tid, ptid):ts)
step (((EVal (VBool True)), env, (SAssert msg Hole):ctx, tid, ptid):ts) _ = evaluate ((SSkip, env, ctx, tid, ptid):ts)
step (((EVal (VBool False)), env, (SAssert msg Hole):ctx, tid, ptid):ts) _ = error $ "Assertion failed: " ++ msg
step ((val, env, (SAssert msg Hole):ctx, tid, ptid):ts) _ = error $ "Cannot assert a non-boolean: "++msg

-- If there are nothing more to parse ignore the evaluate value as it cannot be used anyway
step ((val, env, [], tid, ptid):ts) _ | isValue val = evaluate ((SSkip, env, [], tid, ptid):ts)

step ((e, env, ctx, tid, ptid) : ts) _ = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx


escapeHole :: Env -> [Ctx] -> (Ctx -> Maybe a) -> (a, [Ctx])
escapeHole env ctx f = do
                let octx = dropWhile (isNothing . f) ctx
                let ret = case octx of
                      [] -> error "No context left"
                      (oc:_) -> fromMaybe (error "Failed to escape hole") (f oc)
                (ret, tail octx)

notReset :: Ctx -> Bool
notReset (EReset _) = False
notReset _ = True

firstCall :: Ctx -> Maybe Env
firstCall (ECall (HoleWithEnv e) _ _) = Just e
firstCall (STry (HoleWithEnv e) s cb) = Just e
firstCall _ = Nothing

firstTry :: Ctx -> Maybe (Env, String, Stmt)
firstTry (STry (HoleWithEnv e) s cb) = Just (e, s, cb)
firstTry _ = Nothing

printInfo :: Env -> [Ctx] -> String
printInfo env ctx =  "\n\nContext: " ++ show ctx ++"\n\nEnvironment: "  ++ showNoPrim env