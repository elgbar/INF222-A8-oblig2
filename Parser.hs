module Parser (program, parse) where

import Pretty
import Syntax
import System.IO
import Control.Monad

import Text.Parsec

import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = words "true false var if while fun ref return try catch reset shift spawn detach join import int bool string assert"
           , Token.reservedOpNames = words "+ - * / % == != < > <= >= && || ! ="
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
squares    = Token.brackets   lexer -- parses surrounding square brackets
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses a natural number
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses one of the ops
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses a comma separated list
semiSep1   = Token.semiSep1   lexer -- parses a semi separated list
stringlit = Token.stringLiteral lexer
top = parse program

program = do
  whiteSpace
  ss <- many statement
  eof
  return $ startupCode (foldr SSeq SEof ss)

statement =
  empty <|>
  ifStmt <|>
  whileStmt <|>
  forStmt <|>
  block <|>
  returnStmt <|>

  assertStmt <|>

  tryStmt <|>
  throwStmt <|>

  importStmt <|>

  varDeclStmts <|>
  assignStmt <|>
  exprStmt

empty = semi >> return SSkip
ifStmt = do
  reserved "if"
  e <- parens expr
  s1 <- statement

  el <- optionMaybe (reserved "else")
  s2 <- case el of
    Nothing -> return $ SIf e s1 SSkip
    Just _ -> statement
  return $ SIf e s1 s2

whileStmt = do
  reserved "while"
  e <- parens expr
  SWhile e <$> statement

forStmt = do
  reserved "for"
  [dec, cond, inc] <- between (string "(") (string ")") ( do
          dec <- varDeclStmts
          cond <- expr
          semi
          inc <- statement
          return [dec,cond,inc]
        )
  stmt <- statement
  return $ SBlock $ SSeq dec $ SWhile cond $ SSeq stmt inc

block = do
  ss <- braces (many statement)
  return $ SBlock $ foldr SSeq SSkip ss

assignStmt = do
  i <- try (identifier >>= \j -> reservedOp "=" >> return j)
  e <- expr
  semi
  return $ SAssign i e

namedFun = do
  reserved "fun"
  i <- identifier
  pars <- parens (commaSep identifier)
  body <- block
  return $ SVarDecl i (EFun pars body)

varDeclStmts =
  varDeclStmt "var" expr False <|> --factor already have reference with a factor
  varDeclStmt "bool" boolLiterals True <|> 
  varDeclStmt "int" intLiteral True <|> 
  varDeclStmt "string" stringLiteral True <|>
  namedFun

-- varDeclStmt :: type of statement -> the parser for statement -> if references are allowed -> IO (parsec things)
varDeclStmt typ par ref = do
  reserved typ
  maybeArr <- optionMaybe $ symbol "[" >> symbol "]"
  let arr = case maybeArr of { Just _ -> True; Nothing -> False}
  i <- identifier
  reservedOp "="
  if arr then do
    e <- squares (commaSep (if ref then par <|> refType par else par))
    semi
    return $ SVarDecl i $ EVal $ VArr e
  else do
    e <- if ref then par <|> refType par else par
    semi
    return $ SVarDecl i e

returnStmt = do
  reserved "return"
  e <- optionMaybe expr
  let val = case e of { Just v -> v; Nothing -> EVal VVoid}
  semi
  return $ SReturn val

tryStmt = do
  reserved "try"
  b <- statement
  reserved "catch"
  v <- parens identifier
  c <- statement
  return $ STry b v c
throwStmt = do
  reserved "throw"
  e <- expr
  semi
  return $ SThrow e

importStmt = do
  reserved "import"
  fn <- stringlit
  semi
  return $ SImport $ fn++".impf"

exprStmt = do
  e <- expr
  semi
  return $ SExpr e

assertStmt = do
  reserved "assert"
  -- msg <- stringlit
  e <- expr
  semi
  return $ SAssert (pPrint e) e

expr = conjunction `chainl1` binOp "||"
conjunction = relation `chainl1` binOp "&&"
relation = do
  l <- summation
  (anyBinOp (words "== != < <= > >=") >>= \o -> summation >>= \r -> return $ o l r) <|> return l
summation = term `chainl1` anyBinOp (words "+ -")
term = neg `chainl1` anyBinOp (words "* / %")
neg = (anyUnaOp (words "! -") >>= \o -> factor >>= \r -> return $ o r) <|> factor

factor = literal <|> fun <|> atomicOrCall <|> ref
literal = intLiteral <|> boolLiterals <|> stringLiteral

intLiteral = integer >>= \i -> return $ EVal (VInt (fromInteger i))

boolLiterals = boolLiteral "false" False <|> boolLiteral "true" True
boolLiteral s v = reserved s >> return (EVal (VBool v))

stringLiteral = stringlit >>= \s -> return $ EVal (VString s)

refType typ = reserved "ref" >> typ >>= \e -> return $ ERef e

ref = refType factor
deref = reservedOp "*" >> atomic >>= \e -> return $ EDeref e

binOp s = reservedOp s >> return (\ a b -> ECall (EVar ("__b" ++ s)) [a, b] [])
unaOp s = reservedOp s >> return (\ a -> ECall (EVar ("__u" ++ s)) [a] [])

anyBinOp ops = foldl1 (<|>) (map binOp ops)
anyUnaOp ops = foldl1 (<|>) (map unaOp ops)

fun = do
  reserved "fun"
  pars <- parens (commaSep identifier)
  body <- block
  return $ EFun pars body

variable = do
  i <- identifier
  arr <- optionMaybe $ squares integer
  case arr of 
    Just offset -> return $ EArrVar i (fromIntegral offset)
    Nothing -> return $ EVar i

resetExpr = do
  reserved "reset"
  f <- parens (do
    reserved "fun"
    symbol "("
    symbol ")"
    body <- block
    return $ EFun [] body)
  return $ EReset f
shiftExpr = do 
  reserved "shift"
  f <- parens (do 
    reserved "fun"
    par <- parens identifier
    body <- block
    return $ EFun [par] body)
  return $ EReset f
spawnExpr = do
  reserved "spawn"
  body <- statement
  return $ ESpawn body

detachExpr = do
  reserved "detach"
  tid <- parens factor
  return $ EDetach tid

joinExpr = do
  reserved "join"
  tid <- parens factor
  return $ EJoin tid

atomic = resetExpr <|> shiftExpr <|>
         spawnExpr <|> detachExpr <|> joinExpr <|>
         variable <|> parens expr <|> deref
atomicOrCall = do
  a <- atomic
  args <- many (parens (commaSep expr))
  return $ foldl (\a arg -> ECall a arg []) a args
