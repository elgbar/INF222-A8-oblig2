module Eval where

import Primitive
import Debug.Trace (trace)

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Exception
import System.IO.Unsafe
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

remVar :: String -> Env -> Env
remVar s = filter $ \(n,_) -> n /= s

run :: String -> String -> Env -> Bool -> Bool -> Bool -> IO Env
run input fname env verbose dbg code = 
  case parse program fname input of
    Right v -> do
      when verbose $ putStrLn $ pPrint v
      when code $ print v
      exec v env dbg
    Left e -> do
      putStrLn "Failed to parse file"
      print e
      return env

runInterp :: [String] ->Env -> Bool -> Bool -> Bool -> IO Env
runInterp fnames env verbose dbg code = do
  line <- if null fnames then do
            putStr "> "
            hFlush stdout -- force flushing of std out
            l <-getLine
            return l 
          else do
            sequence_ [putStrLn $ "Loading " ++ show (removeSub ".impf" fname) | fname <- fnames]
            return $ concatMap (\fname -> 
                          let importName = removeSub ".impf" fname
                          in "import "++show importName++";") fnames
  ran <- ecTry $ run line "<console>" env verbose dbg code 
  env' <- case ran of
    Right env' -> return env'
    Left err -> print err >> return env
  runInterp [] env' verbose dbg code

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
    running <- filterM (\f -> case f of {(SSkip, _, [], n, _) -> return False ; _ -> return True}) threads -- filter out ended threads
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
    (SSkip, _, [], _, _) -> evaluate $ if threadId t == 0 then [t] else ts
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
step ((v, env, SVarDecl s Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = expr2val v
  case val of 
    -- no need to evaluate if no elements
    (VArr arr) -> evaluate $ (SSkip, env, SVarDeclRef s arr [] : ctx, tid, ptid) : ts
    val -> evaluate ((SSkip, addVar s val env, ctx, tid, ptid):ts)

step ((SSkip, env, SVarDeclRef s [] done : ctx, tid, ptid) : ts) _ = 
  evaluate ((SSkip, addVar s (VArr done) env, ctx, tid, ptid):ts)

step ((SSkip, env, SVarDeclRef s todo done : ctx, tid, ptid) : ts) _ = 
  case head todo of
    e@(ERef val) -> evaluate $ (e, env, SVarDeclRef s (tail todo) done : ctx, tid, ptid) : ts
    val ->  evaluate $ (SSkip, env, SVarDeclRef s (tail todo) (done ++ [val]) : ctx, tid, ptid) : ts

step ((val, env, SVarDeclRef s todo done : ctx, tid, ptid) : ts) _ | isValue val= 
  evaluate $ (SSkip, env, SVarDeclRef s todo (done ++ [val]) : ctx, tid, ptid) : ts

-- Delete a variable
step ((SDelete s, env, ctx, tid, ptid) : ts) _ = evaluate ((SSkip, remVar s env, ctx, tid, ptid):ts)

-- Assignment
step ((SAssign s e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, SAssign s Hole : ctx, tid, ptid):ts)
step ((v, env, SAssign s Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  writeRef (findVar s env) (expr2val v) s
  evaluate ((SSkip, env, ctx, tid, ptid):ts)

step ((SArrAssign s i e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, SArrAssign s i Hole : ctx, tid, ptid):ts)
step ((v, env, SArrAssign s i Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let var = findVar s env
  arr <- arrVal2Arr s var
  let elem = expr2val $ arr !! i -- old element 
  let val = expr2val v -- new element
  writeRef elem val s
  evaluate ((SSkip, env, ctx, tid, ptid):ts)

-- Variable reference: get from environment
step ((EVar s, env, ctx, tid, ptid) : ts) _ = 
  case findVar s env of 
    v -> evaluate ((EVal v, env, ctx, tid, ptid):ts)

step ((EArrVar s i, env, ctx, tid, ptid) : ts) _ =  evaluate ((i, env, EArrVar s Hole:ctx, tid, ptid) : ts)
step ((ev, env, EArrVar s Hole : ctx, tid, ptid) : ts) _ | isValue ev =
  case expr2val ev of
      (VInt i) -> 
          case findVar s env of
            a@(VRef _ (VArr _)) -> do
              arr <- arrVal2Arr s a
              evaluate ((arr !! i, env, ctx, tid, ptid):ts)
            (VArr arr) -> evaluate ((arr !! i, env, ctx, tid, ptid):ts)
            (VString arr) -> evaluate ((EVal $ VString [arr !! i], env, ctx, tid, ptid):ts)
            v -> error $ "Expected to find an array or string but varible "++show s ++" is "++val2type v
      e -> error $ "Array value lookup only accepts integer as keys. Got "++show e

-- Box a value
step ((ERef e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, ERef Hole : ctx, tid, ptid):ts)
step ((v, env, ERef Hole : ctx, tid, ptid) : ts) _ | isValue v = do
  let val = case expr2val v of
              VArr xs -> VArr (writeRefs xs)
              v -> v
  nv <- newIORef val
  evaluate ((EVal (VRef nv val), env, ctx, tid, ptid):ts)

-- Dereference a ref
step ((EDeref e, env, ctx, tid, ptid) : ts) _ = evaluate ((e, env, EDeref Hole : ctx, tid, ptid):ts)
step ((e, env, EDeref Hole : ctx, tid, ptid) : ts) _ | isValue e = do
  let val = expr2val e
  rval <- readRef val
  case rval of
    Just v -> evaluate $ (EVal v, env, ctx, tid, ptid):ts
    Nothing -> error $ "Trying to dereference a non-ref " ++ show val

-- Function becomes a closure
step ((EFun pars body, env, ctx, tid, ptid) : ts) _ = evaluate ((EVal $ VClosure pars body env, env, ctx, tid, ptid):ts)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step ((ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, tid, ptid) : ts) _ = evaluate ((s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, tid, ptid):ts)
step ((SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, tid, ptid) : ts) _ = evaluate ((EVal VVoid, env, ctx, tid, ptid):ts)
  -- function body fully evaluated, evaluate VVoid

step ((ECall (EVal (VPrimFun n f)) [] vs, env, ctx, tid, ptid) : ts) _ = do
  let rvs = reverse vs
  val <- pmfTry $ evaluate (f rvs)
  case val of
    Right v -> evaluate ((EVal v, env, ctx, tid, ptid):ts)
    Left e -> error $ "Invalid arguments for the primitive function " ++ show n ++ " "++ show rvs++" types: "++ show (map val2type rvs)

step ((ECall (EVal (VPrimFunIO n f)) [] vs, env, ctx, tid, ptid) : ts) _ = do
  let rvs = reverse vs
  val  <- pmfTry $ f rvs
  case val of
    Right v -> evaluate ((EVal v, env, ctx, tid, ptid):ts)
    Left e -> error $ "Invalid arguments for the primitive IO function " ++ show n ++ " "++ show rvs++" types: "++ show (map val2type rvs)

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
step ((v@(EVal VClosure{}), env, EReset Hole : ctx, tid, ptid) : ts) _ =
     evaluate $ (ECall v [] [], env, EReset Hole : ctx, tid, ptid) : ts

step ((EShift f, env, ctx, tid, ptid) : ts) _  = do
  let nctx = takeWhile notReset ctx
  evaluate $ (f, env, EVal (VCont env nctx) : ctx, tid, ptid) : ts
step ((v@(EVal VClosure{}), env, EVal (VCont nenv nctx) : ctx, tid, ptid) : ts) _ = 
  evaluate $ (ECall v [] [], nenv, nctx, tid, ptid) : ts

step ((SAssert msg e, env, ctx, tid, ptid):ts) _ = evaluate ((e, env, SAssert msg Hole : ctx, tid, ptid):ts)
step ((EVal (VBool True), env, SAssert msg Hole : ctx, tid, ptid):ts) _ = evaluate ((SSkip, env, ctx, tid, ptid):ts)
step ((EVal (VBool False), env, SAssert msg Hole : ctx, tid, ptid):ts) _ = error $ "Assertion failed: " ++ msg
step ((val, env, SAssert msg Hole : ctx, tid, ptid):ts) _ = error $ "Cannot assert a non-boolean: "++msg

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

arrVal2Arr :: String -> Value -> IO [Expr]
arrVal2Arr s var =
  case var of
    (VRef nv ot) -> do
      val <- readIORef nv
      case val  of 
        VArr arr -> return arr
        _ -> error "uaa"
    (VArr arr) -> return arr
    _ -> error $ "Trying to assign array but found non-array " ++ show s ++ " of type " ++ show (val2type var)