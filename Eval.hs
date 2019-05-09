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

      -- /(frame info), frame id, parent id
type Frame = (Ast, Env, [Ctx], Int, Int)

getNextId :: [Frame] -> Int
getNextId frames = foldl max 0 (map frameId frames) + 1 

frameExists :: Int -> [Frame] -> Bool
frameExists fid frames = fid `elem` map frameId frames

frameId :: Frame -> Int
frameId (_, _, _, f, _) = f

parentId :: Frame -> Int
parentId (_, _, _, _, p) = p

-- getFrame :: Int -> [Frame] -> Frame
-- getFrame fid frames = undefined--head $ filter ()

exec :: Ast -> Bool -> IO Env
exec e dbg = steps [(e, primitives, [], 0, 0)] dbg
  -- do
  -- (_, env, _, 0, 0) <- steps [(e, primitives, [], 0, 0)] dbg
  -- return env

concatMapM :: (a -> IO [a]) -> [a] -> IO [a]
concatMapM f list = fmap concat (mapM f list)

      -- frames to run -> frames ran, _ _ -> new state
steps :: [Frame] -> Bool -> IO Env
--fmap concat (
steps frs dbg  = do
    frames <- concatMapM (\f -> step f frs dbg) frs --mapM (`step` dbg) frs 
    -- let main = getFrame 0 frames
    nonDead <- filterM (\f -> case f of {(SSkip, _, [], n, _) -> return $ n == 0 ; otherwise -> return True}) frames -- filter out ended frames
    alive <- filterM (\f -> return $ frameExists (parentId f) nonDead) nonDead -- filter out frames where the parent has stopped
    case alive of
      [] -> error "No threads alive"
      (SSkip, env, [], 0, 0):s -> return env
      [(SSkip, _, [], _, _)] -> error "Main thread not last alive"
      _ -> steps alive dbg 


step :: Frame -> [Frame] -> Bool -> IO [Frame]
step (ast, e, c, fid, pfid) _ True | trace ("Evaluating (tid "++show fid++", pid "++show pfid++")\nast: "++ show ast ++ "\n\nctx: " ++ show c ++"\n\nenv: "++ showNoPrim e ++"\n\n\n") False = undefined

-- step fr@(SSkip, _, [], _, _) _ _ = return [fr]

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx, fid, pfid) _ _ = return [(e, env, SExpr Hole : ctx, fid, pfid)]
step (v, env, SExpr Hole : ctx, fid, pfid) _ _ | isValue v = return [(SSkip, env, ctx, fid, pfid)]

-- Blocks
step (SBlock s, env, ctx, fid, pfid) _ _ = return [(s, env, SBlock (HoleWithEnv env) : ctx, fid, pfid)]
step (SSkip, _, SBlock (HoleWithEnv env) : ctx, fid, pfid) _ _ = return [(SSkip, env, ctx, fid, pfid)] -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx, fid, pfid) _ _ = return [(s1, env, SSeq Hole s2 : ctx, fid, pfid)]
step (SSkip, env, SSeq Hole s2 : ctx, fid, pfid) _ _ = return [(s2, env, ctx, fid, pfid)]

-- If and while
step (SIf cond s1 s2, env, ctx, fid, pfid) _ _ = return [(cond, env, SIf Hole s1 s2 : ctx, fid, pfid)]
step (EVal (VBool True), env, SIf Hole s1 _ : ctx, fid, pfid) _ _ = return [(SBlock s1, env, ctx, fid, pfid)]
step (EVal (VBool False), env, SIf Hole _ s2 : ctx, fid, pfid) _ _ = return [(SBlock s2, env, ctx, fid, pfid)]

step (w@(SWhile cond s), env, ctx, fid, pfid) _ _ = return [(SIf cond (SSeq s w) SSkip, env, ctx, fid, pfid)]

-- Variable declaration
step (SVarDecl s e, env, ctx, fid, pfid) _ _ = return [(e, env, SVarDecl s Hole : ctx, fid, pfid)]
step (v, env, SVarDecl s Hole : ctx, fid, pfid) _ _ | isValue v
  = return [(SSkip, addVar s (expr2val v) env, ctx, fid, pfid)]

-- Assignment
step (SAssign s e, env, ctx, fid, pfid) _ _ = return [(e, env, SAssign s Hole : ctx, fid, pfid)]
step (v, env, SAssign s Hole : ctx, fid, pfid) _ _ | isValue v =
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return [(SSkip, env, ctx, fid, pfid)]
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step (EVar s, env, ctx, fid, pfid) _ _ = return [(EVal $ findVar s env, env, ctx, fid, pfid)]

-- Box a value
step (ERef e, env, ctx, fid, pfid) _ _ = return [(e, env, ERef Hole : ctx, fid, pfid)]
step (v, env, ERef Hole : ctx, fid, pfid) _ _ | isValue v = do
  nv <- newIORef (expr2val v)
  return [(EVal (VRef nv), env, ctx, fid, pfid)]

-- Dereference a ref
step (EDeref e, env, ctx, fid, pfid) _ _ = return [(e, env, EDeref Hole : ctx, fid, pfid)]
step (v, env, EDeref Hole : ctx, fid, pfid) _ _ | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return [(EVal v', env, ctx, fid, pfid)]

-- Function becomes a closure
step (EFun pars body, env, ctx, fid, pfid) _ _ = return [(EVal $ VClosure pars body env, env, ctx, fid, pfid)]

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, fid, pfid) _ _ = return [(s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, fid, pfid)]
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, fid, pfid) _ _ = return [(EVal VVoid, env, ctx, fid, pfid)]
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx, fid, pfid) _ _ = return [(EVal $ f (reverse vs), env, ctx, fid, pfid)]
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, fid, pfid) _ _ = do
  res  <- f (reverse vs)
  return [(EVal res, env, ctx, fid, pfid)]
step (ECall f [] _, _, _, _, _) _ _ | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function posi tion
step (ECall f args [], env, ctx, fid, pfid) _ _ | notValue f = return [(f, env, ECall Hole args [] : ctx, fid, pfid)]
step (f, env, ECall Hole args [] : ctx, fid, pfid) _ _ | isValue f = return [(ECall f args [], env, ctx, fid, pfid)]
step (ECall f (a:args) vs, env, ctx, fid, pfid) _ _ | isValue f = return [(a, env, ECall f (Hole:args) vs : ctx, fid, pfid)]
step (v, env, ECall f (Hole:args) vs : ctx, fid, pfid) _ _ | isValue v = return [(ECall f args (expr2val v : vs), env, ctx, fid, pfid)]

-- return
step (SReturn Hole, env, v:ctx, fid, pfid) _ _ | notValue v  = return [(v, env, ctx, fid, pfid)] -- evaluate return expr to value (note/warn: what if never value?)
step (val@(EVal _), env, SReturn Hole : ctx, fid, pfid) _ _ = -- must be value
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  return [(val, oenv, octx, fid, pfid)]
step (SReturn val, env, ctx, fid, pfid) _ _ = -- initial evaluation, parse the maybe expr and put it on the stack/ctx
  return [(val, env, SReturn Hole : ctx, fid, pfid)] --put what to return (val) on the stack
step (EVal VVoid, env, ctx, fid, pfid) _ _ = return [(SSkip, env, ctx, fid, pfid)] --toplevel return [(hopefully)

--Throw
step (SThrow Hole, env, v:ctx, fid, pfid) _ _ | notValue v  = return [(v, env, ctx, fid, pfid)] -- evaluate return expr to value (note/warn: what if never value?)
step (SThrow val, env, ctx, fid, pfid) _ _ = return [(val, env, SThrow Hole : ctx, fid, pfid)] --eval expr to value before catching

--Try
step (STry b v c, env, ctx, fid, pfid) _ _ = return [(b, env, STry (HoleWithEnv env) v c:ctx, fid, pfid)] -- entering a try block
--catch

step (EVal v, env, SThrow Hole : ctx, fid, pfid) _ _ = -- must be value
  let ((s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  return [(blk, addVar s v env, octx, fid, pfid)]
step (SSkip, env, STry (HoleWithEnv e) _ _:ctx, fid, pfid) _ dbg = return [(SSkip, e, ctx, fid, pfid)] -- nothing was thrown

-- import : replace import with this one
step (SImport fn, oldEnv, ctx, fid, pfid) _ dbg = do
   s <- readFile fn
   let env = unsafePerformIO $ run s fn dbg dbg -- should verbosity be passed to step?
   return [(SSkip, oldEnv ++ env, ctx, fid, pfid)] --it hurts me using the unsafeIO
step (SEof, env, ctx, fid, pfid) _ _ = return [(SSkip, env, [], fid, pfid)] -- reached end of file, its here to pass env after an import

step (ESpawn e, env, ctx, fid, pfid) frames _  = do
  let nfid = getNextId frames
  return [(EVal (VInt nfid), env, ctx, fid, pfid), (e, env, [], nfid, fid)]

step (EDetach e, env, ctx, fid, pfid) _ _ | notValue e = return [(e, env, EDetach Hole : ctx, fid, pfid)] -- parse expr
step (EVal (VInt n), env, EDetach Hole : ctx, fid, pfid) frames _ = --do
  
  --  alive <- mapM (\frm@(a,e,c,f,p) -> if f == n then (a,e,c,n,p) else frm) frames -- filter out frames where the parent has stopped
   return [(SSkip, env, ctx, fid, 0)]
step (EVal v, env, EDetach Hole : ctx, fid, pfid) _ _ = error "Thread ids can only be integers"

step (EJoin e, env, ctx, fid, pfid) _ _ | notValue e = return [(e,env,EJoin Hole : ctx,fid,pfid)] -- parse expr
step f@(EVal (VInt n), env, EJoin Hole : ctx, fid, pfid) frames _ | frameExists n frames = return [f] -- given frame id still exists so we continue to wait
step (EVal (VInt n), env, EJoin Hole : ctx, fid, pfid) frames _ | not $ frameExists n frames = return [(SSkip, env, ctx, fid, pfid)]
step (EVal v, env, EJoin Hole : ctx, fid, pfid) _ _ = error "Thread ids can only be integers"

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (e, env, ctx, fid, pfid) _ _ = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx

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
