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

parentExists :: Frame -> [Frame] -> IO Bool
parentExists f frames = return $ parentId f `elem` map frameId frames

frameId :: Frame -> Int
frameId (_, _, _, f, _) = f

parentId :: Frame -> Int
parentId (_, _, _, _, p) = p

getFrame :: Int -> [Frame] -> Frame
getFrame fid frames = undefined--head $ filter ()

exec :: Ast -> Bool -> IO Env
exec e dbg = steps [(e, primitives, [], 0, 0)] dbg
  -- do
  -- (_, env, _, 0, 0) <- steps [(e, primitives, [], 0, 0)] dbg
  -- return env

concatMapM :: (a -> IO [a]) -> [a] -> IO [a]
concatMapM f list = fmap concat (mapM f list)

      -- frames to run -> frames ran, _ dbg  -> new state
steps :: [Frame] -> Bool -> IO Env
--fmap concat (
steps frs dbg  = do
    frames <- concatMapM (\f -> step f frs dbg) frs --mapM (`step` dbg) frs 
    -- let main = getFrame 0 frames
    nonDead <- filterM (\f -> case f of {(SSkip, _, [], _, _) -> return False ; otherwise -> return True}) frames -- filter out ended frames
    alive <- filterM (`parentExists` frames) frames -- filter out frames where the parent has stopped
    case alive of
      [(SSkip, env, [], 0, 0)] -> return env
      _ -> steps alive dbg 


step :: Frame -> [Frame] -> Bool -> IO [Frame]
step (ast, e, c, fr, pr) _ True | trace ("Evaluating (frame "++show fr++", parent "++show pr++")\nast: "++ show ast ++ "\n\nctx: " ++ show c ++"\n\nWith the Env: "++ showNoPrim e ++"\n\n\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx, fr, pr) _ dbg  = return [(e, env, SExpr Hole : ctx, fr, pr)]
step (v, env, SExpr Hole : ctx, fr, pr) _ dbg  | isValue v = return [(SSkip, env, ctx, fr, pr)]

-- Blocks
step (SBlock s, env, ctx, fr, pr) _ dbg  = return [(s, env, SBlock (HoleWithEnv env) : ctx, fr, pr)]
step (SSkip, _, SBlock (HoleWithEnv env) : ctx, fr, pr) _ dbg  = return [(SSkip, env, ctx, fr, pr)] -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx, fr, pr) _ dbg  = return [(s1, env, SSeq Hole s2 : ctx, fr, pr)]
step (SSkip, env, SSeq Hole s2 : ctx, fr, pr) _ dbg  = return [(s2, env, ctx, fr, pr)]

-- If and while
step (SIf cond s1 s2, env, ctx, fr, pr) _ dbg  = return [(cond, env, SIf Hole s1 s2 : ctx, fr, pr)]
step (EVal (VBool True), env, SIf Hole s1 _ : ctx, fr, pr) _ dbg  = return [(SBlock s1, env, ctx, fr, pr)]
step (EVal (VBool False), env, SIf Hole _ s2 : ctx, fr, pr) _ dbg  = return [(SBlock s2, env, ctx, fr, pr)]

step (w@(SWhile cond s), env, ctx, fr, pr) _ dbg  = return [(SIf cond (SSeq s w) SSkip, env, ctx, fr, pr)]

-- Variable declaration
step (SVarDecl s e, env, ctx, fr, pr) _ dbg  = return [(e, env, SVarDecl s Hole : ctx, fr, pr)]
step (v, env, SVarDecl s Hole : ctx, fr, pr) _ dbg  | isValue v
  = return [(SSkip, addVar s (expr2val v) env, ctx, fr, pr)]

-- Assignment
step (SAssign s e, env, ctx, fr, pr) _ dbg  = return [(e, env, SAssign s Hole : ctx, fr, pr)]
step (v, env, SAssign s Hole : ctx, fr, pr) _ dbg  | isValue v =
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return [(SSkip, env, ctx, fr, pr)]
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""


-- Variable reference: get from environment
step (EVar s, env, ctx, fr, pr) _ dbg  = return [(EVal $ findVar s env, env, ctx, fr, pr)]

-- Box a value
step (ERef e, env, ctx, fr, pr) _ dbg  = return [(e, env, ERef Hole : ctx, fr, pr)]
step (v, env, ERef Hole : ctx, fr, pr) _ dbg  | isValue v = do
  nv <- newIORef (expr2val v)
  return [(EVal (VRef nv), env, ctx, fr, pr)]

-- Dereference a ref
step (EDeref e, env, ctx, fr, pr) _ dbg  = return [(e, env, EDeref Hole : ctx, fr, pr)]
step (v, env, EDeref Hole : ctx, fr, pr) _ dbg  | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return [(EVal v', env, ctx, fr, pr)]

-- Function becomes a closure
step (EFun pars body, env, ctx, fr, pr) _ dbg  = return [(EVal $ VClosure pars body env, env, ctx, fr, pr)]

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx, fr, pr) _ dbg  = return [(s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx, fr, pr)]
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx, fr, pr) _ dbg  = return [(EVal VVoid, env, ctx, fr, pr)]
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx, fr, pr) _ dbg  = return [(EVal $ f (reverse vs), env, ctx, fr, pr)]
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx, fr, pr) _ dbg  = do
  res  <- f (reverse vs)
  return [(EVal res, env, ctx, fr, pr)]
step (ECall f [] _, _, _, _, _) _ _ | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function posi tion
step (ECall f args [], env, ctx, fr, pr) _ dbg  | notValue f = return [(f, env, ECall Hole args [] : ctx, fr, pr)]
step (f, env, ECall Hole args [] : ctx, fr, pr) _ dbg  | isValue f = return [(ECall f args [], env, ctx, fr, pr)]
step (ECall f (a:args) vs, env, ctx, fr, pr) _ dbg  | isValue f = return [(a, env, ECall f (Hole:args) vs : ctx, fr, pr)]
step (v, env, ECall f (Hole:args) vs : ctx, fr, pr) _ dbg  | isValue v = return [(ECall f args (expr2val v : vs), env, ctx, fr, pr)]

-- return
step (SReturn Hole, env, v:ctx, fr, pr) _ dbg  | notValue v  = return [(v, env, ctx, fr, pr)] -- evaluate return expr to value (note/warn: what if never value?)
step (val@(EVal _), env, SReturn Hole : ctx, fr, pr) _ dbg  = -- must be value
  let (oenv, octx) = escapeHole env ctx firstCall in -- find nearest escape (other env (note: maybe only find next hole?))
  return [(val, oenv, octx, fr, pr)]
step (SReturn val, env, ctx, fr, pr) _ dbg  = -- initial evaluation, parse the maybe expr and put it on the stack/ctx
  return [(val, env, SReturn Hole : ctx, fr, pr)] --put what to return [(val) on the stack
step (EVal VVoid, env, ctx, fr, pr) _ dbg  = return [(SSkip, env, ctx, fr, pr)] --toplevel return [(hopefully)

--Throw
step (SThrow Hole, env, v:ctx, fr, pr) _ dbg  | notValue v  = return [(v, env, ctx, fr, pr)] -- evaluate return expr to value (note/warn: what if never value?)
step (SThrow val, env, ctx, fr, pr) _ dbg  = return [(val, env, SThrow Hole : ctx, fr, pr)] --eval expr to value before catching

--Try
step (STry b v c, env, ctx, fr, pr) _ dbg  = return [(b, env, STry (HoleWithEnv env) v c:ctx, fr, pr)] -- entering a try block
--catch

step (EVal v, env, SThrow Hole : ctx, fr, pr) _ dbg  = -- must be value
  let ((s, blk), octx) = escapeHole env ctx firstTry in -- find nearest escape (other env)
  return [(blk, addVar s v env, octx, fr, pr)]
step (SSkip, env, STry (HoleWithEnv e) _ _:ctx, fr, pr) _ dbg = return [(SSkip, e, ctx, fr, pr)] -- nothing was thrown

-- import : replace import with this one
step (SImport fn, oldEnv, ctx, fr, pr) _ dbg  = do
   s <- readFile fn
   let env = unsafePerformIO $ run s fn dbg dbg -- should verbosity be passed to step?
   return [(SSkip, oldEnv ++ env, ctx, fr, pr)] --it hurts me using the unsafeIO
step (SEof, env, ctx, fr, pr) _ dbg  = return [(SSkip, env, [], fr, pr)] -- reached end of file, its here to pass env after an import

step frame@(ESpawn e, env, ctx, fr, pr) frames dbg  = 
  return [frame, (e, primitives, [], getNextId frames, fr)]

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (e, env, ctx, fr, pr) _ dbg  = error $ "Stuck at expression: " ++ show e ++ printInfo env ctx

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
