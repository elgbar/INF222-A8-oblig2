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
getNextId frames = foldl max 0 (map frameId frames) + 1 

frameExists :: Int -> [Thread] -> Bool
frameExists tid frames = tid `elem` map frameId frames

frameId :: Thread -> Int
frameId (_, _, _, f, _) = f

parentId :: Thread -> Int
parentId (_, _, _, _, p) = p

-- getFrame :: Int -> [Thread] -> Thread
-- getFrame tid [] = error $ "Failed to find a thread with the ID "++tid
-- getFrame tid frames = 

exec :: Ast -> Bool -> IO Env
exec e = steps [(e, primitives, [], 0, 0)]
  -- do
  -- (_, env, _, 0, 0) <- steps [(e, primitives, [], 0, 0)] dbg
  -- return env

concatMapM :: (a -> IO [a]) -> [a] -> IO [a]
concatMapM f list = fmap concat (mapM f list)

      -- frames to run -> frames ran, _ _ -> new state
steps :: [Thread] -> Bool -> IO Env
--fmap concat (
steps frs dbg  = do

    frames <- concatMapM (\f -> step f frs dbg) frs -- step in each thread
    nonDead <- filterM (\f -> case f of {(SSkip, _, [], n, _) -> return $ n == 0 ; otherwise -> return True}) frames -- filter out ended frames
    alive <- filterM (\f -> return $ frameExists (parentId f) nonDead) nonDead -- filter out frames where the parent has stopped
    case alive of
      [] -> error "No threads alive"
      [(SSkip, _, [], _, _)] -> error "Main thread not last alive"
      (SSkip, env, [], 0, 0):s -> return env
      _ -> steps alive dbg 


step :: Thread -> [Thread] -> Bool -> IO [Thread]
step (ast, e, c, tid, ptid) _ True | trace ("Evaluating (tid "++show tid++", pid "++show ptid++")\nast: "++ show ast ++ "\n\nctx: " ++ show c ++"\n\nenv: "++ showNoPrim e ++"\n\n\n") False = undefined

-- step fr@(SSkip, _, [], _, _) _ _ = return [fr]

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx, tid, ptid) _ _ = return [(e, env, SExpr Hole : ctx, tid, ptid)]
step (v, env, SExpr Hole : ctx, tid, ptid) _ _ | isValue v = return [(SSkip, env, ctx, tid, ptid)]

-- Blocks
step (SBlock s, env, ctx, tid, ptid) _ _ = return [(s, env, SBlock (HoleWithEnv env) : ctx, tid, ptid)]
step (SSkip, _, SBlock (HoleWithEnv env) : ctx, tid, ptid) _ _ = return [(SSkip, env, ctx, tid, ptid)] -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx, tid, ptid) _ _ = return [(s1, env, SSeq Hole s2 : ctx, tid, ptid)]
step (SSkip, env, SSeq Hole s2 : ctx, tid, ptid) _ _ = return [(s2, env, ctx, tid, ptid)]

-- If and while
step (SIf cond s1 s2, env, ctx, tid, ptid) _ _ = return [(cond, env, SIf Hole s1 s2 : ctx, tid, ptid)]
step (EVal (VBool True), env, SIf Hole s1 _ : ctx, tid, ptid) _ _ = return [(SBlock s1, env, ctx, tid, ptid)]
step (EVal (VBool False), env, SIf Hole _ s2 : ctx, tid, ptid) _ _ = return [(SBlock s2, env, ctx, tid, ptid)]

step (w@(SWhile cond s), env, ctx, tid, ptid) _ _ = return [(SIf cond (SSeq s w) SSkip, env, ctx, tid, ptid)]

-- Variable declaration
step (SVarDecl s e, env, ctx, tid, ptid) _ _ = return [(e, env, SVarDecl s Hole : ctx, tid, ptid)]
step (v, env, SVarDecl s Hole : ctx, tid, ptid) _ _ | isValue v
  = return [(SSkip, addVar s (expr2val v) env, ctx, tid, ptid)]

-- Assignment
step (SAssign s e, env, ctx, tid, ptid) _ _ = return [(e, env, SAssign s Hole : ctx, tid, ptid)]
step (v, env, SAssign s Hole : ctx, tid, ptid) _ _ | isValue v =
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return [(SSkip, env, ctx, tid, ptid)]
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step (EVar s, env, ctx, tid, ptid) _ _ = return [(EVal $ findVar s env, env, ctx, tid, ptid)]

-- Box a value
step (ERef e, env, ctx, tid, ptid) _ _ = return [(e, env, ERef Hole : ctx, tid, ptid)]
step (v, env, ERef Hole : ctx, tid, ptid) _ _ | isValue v = do
  nv <- newIORef (expr2val v)
  return [(EVal (VRef nv), env, ctx, tid, ptid)]

-- Dereference a ref
step (EDeref e, env, ctx, tid, ptid) _ _ = return [(e, env, EDeref Hole : ctx, tid, ptid)]
step (v, env, EDeref Hole : ctx, tid, ptid) _ _ | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return [(EVal v', env, ctx, tid, ptid)]

-- Function becomes a closure
step (EFun pars body, env, ctx, tid, ptid) _ _ = return [(EVal $ VClosure pars body env, env, ctx, tid, ptid)]

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, tid, ptid) _ _ = return [(s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, tid, ptid)]
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, tid, ptid) _ _ = return [(EVal VVoid, env, ctx, tid, ptid)]
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx, tid, ptid) _ _ = return [(EVal $ f (reverse vs), env, ctx, tid, ptid)]
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, tid, ptid) _ _ = do
  res  <- f (reverse vs)
  return [(EVal res, env, ctx, tid, ptid)]
step (ECall f [] _, _, _, _, _) _ _ | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function posi tion
step (ECall f args [], env, ctx, tid, ptid) _ _ | notValue f = return [(f, env, ECall Hole args [] : ctx, tid, ptid)]
step (f, env, ECall Hole args [] : ctx, tid, ptid) _ _ | isValue f = return [(ECall f args [], env, ctx, tid, ptid)]
step (ECall f (a:args) vs, env, ctx, tid, ptid) _ _ | isValue f = return [(a, env, ECall f (Hole:args) vs : ctx, tid, ptid)]
step (v, env, ECall f (Hole:args) vs : ctx, tid, ptid) _ _ | isValue v = return [(ECall f args (expr2val v : vs), env, ctx, tid, ptid)]

-- return
step (SReturn Hole, env, v:ctx, tid, ptid) _ _ | notValue v  = return [(v, env, ctx, tid, ptid)] -- evaluate return expr to value (note/warn: what if never value?)
step (val@(EVal _), env, SReturn Hole : ctx, tid, ptid) _ _ = -- must be value
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  return [(val, oenv, octx, tid, ptid)]
step (SReturn val, env, ctx, tid, ptid) _ _ = -- initial evaluation, parse the maybe expr and put it on the stack/ctx
  return [(val, env, SReturn Hole : ctx, tid, ptid)] --put what to return (val) on the stack
step (EVal VVoid, env, ctx, tid, ptid) _ _ = return [(SSkip, env, ctx, tid, ptid)] --toplevel return [(hopefully)

--Throw
step (SThrow Hole, env, v:ctx, tid, ptid) _ _ | notValue v  = return [(v, env, ctx, tid, ptid)] -- evaluate return expr to value (note/warn: what if never value?)
step (SThrow val, env, ctx, tid, ptid) _ _ = return [(val, env, SThrow Hole : ctx, tid, ptid)] --eval expr to value before catching

--Try
step (STry b v c, env, ctx, tid, ptid) _ _ = return [(b, env, STry (HoleWithEnv env) v c:ctx, tid, ptid)] -- entering a try block
--catch

step (EVal v, env, SThrow Hole : ctx, tid, ptid) _ _ = -- must be value
  let ((s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  return [(blk, addVar s v env, octx, tid, ptid)]
step (SSkip, env, STry (HoleWithEnv e) _ _:ctx, tid, ptid) _ dbg = return [(SSkip, e, ctx, tid, ptid)] -- nothing was thrown

-- import : replace import with this one
step (SImport fn, oldEnv, ctx, tid, ptid) _ dbg = do
   s <- readFile fn
   let env = unsafePerformIO $ run s fn dbg dbg -- should verbosity be passed to step?
   return [(SSkip, oldEnv ++ env, ctx, tid, ptid)] --it hurts me using the unsafeIO
step (SEof, env, ctx, tid, ptid) _ _ = return [(SSkip, env, [], tid, ptid)] -- reached end of file, its here to pass env after an import

step (ESpawn e, env, ctx, tid, ptid) frames _  = do
  let nfid = getNextId frames
  return [(EVal (VInt nfid), env, ctx, tid, ptid), (e, env, [], nfid, tid)]

step (EDetach e, env, ctx, tid, ptid) _ _ | notValue e = return [(e, env, EDetach Hole : ctx, tid, ptid)] -- parse expr
step (EVal (VInt n), env, EDetach Hole : ctx, tid, ptid) frames _ = --do
  
  --  alive <- mapM (\frm@(a,e,c,f,p) -> if f == n then (a,e,c,n,p) else frm) frames -- filter out frames where the parent has stopped
   return [(SSkip, env, ctx, tid, 0)]
step (EVal v, env, EDetach Hole : ctx, tid, ptid) _ _ = error "Thread ids can only be integers"

step (EJoin e, env, ctx, tid, ptid) _ _ | notValue e = return [(e,env,EJoin Hole : ctx,tid,ptid)] -- parse expr
step f@(EVal (VInt n), env, EJoin Hole : ctx, tid, ptid) frames _ | frameExists n frames = return [f] -- given thread id still exists so we continue to wait
step (EVal (VInt n), env, EJoin Hole : ctx, tid, ptid) frames _ | not $ frameExists n frames = return [(SSkip, env, ctx, tid, ptid)]
step (EVal v, env, EJoin Hole : ctx, tid, ptid) _ _ = error "Thread ids can only be integers"

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (e, env, ctx, tid, ptid) _ _ = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx

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
printInfo env ctx =  "\n\nContext: " ++ show ctx ++"\n\nEnvironment: "  ++ showNoPrim env 
