module Pretty
  ( pPrint
  ) where

import           Prelude          hiding ((<>))
import           Syntax
import           Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

pPrint :: Pretty a => a -> String
pPrint t = render $ pp t

instance Pretty Ast where
  pp Hole = text "[]"
  pp (HoleWithEnv _) = text "[env]"
  pp SSkip = text ";"
  pp (SIf e s1 s2) =
    text "if (" <> pp e <> text ") {" $+$ nest 2 (pp s1) $+$
    text "} else {" $+$ nest 2 (pp s2) $+$ text "}"
  pp (SWhile e s) =
    text "while (" <> pp e <> text ") {" $+$ nest 2 (pp s) $+$ text "}"
  pp (SExpr e) = pp e <> text ";"
  pp (SBlock s) = text "{" $+$ nest 2 (pp s) $$ text "}"
  pp s@(SSeq s1 s2) = vcat (map pp (seq2list s))
    where
      seq2list (SSeq s1 s2) = s1 : seq2list s2
      seq2list s            = [s]
  pp (SVarDecl s e) = text "var" <+> text s <+> text "=" <+> pp e <> text ";"
  pp (SAssign s e) = text s <+> text "=" <+> pp e <> text ";"
  pp (SArrAssign s i e) = text s <> text ("[" ++ show i ++ "]") <+> text "=" <+> pp e <> text ";"
  pp (SReturn (EVal VVoid)) = text "return"
  pp (SReturn v) = text "return" <+> pp v
  pp (EVal v) = pp v
  pp (EVar s) = text s
  pp (EArrVar s i) = text $ s ++ "[" ++ show i ++ "]"
  pp (EFun ss s) =
    text "fun(" <> hcat (punctuate comma (map text ss)) <> text ")" <+> pp s
  pp (ECall e es vs) =
    pp e <> text "(" <> hcat (punctuate comma (map pp es ++ map pp vs)) <> text ")"
  pp (EDeref e) = text "*" <> pp e
  pp (ERef e) = text "ref" <+> pp e
  pp (SThrow msg) = text "throw" <+> pp msg
  pp (STry blk var cblk) =
    text "try {" $+$ nest 2 (pp blk) $+$ text "} catch(" <+> text var <+> text ") {" $+$ nest 2 (pp cblk) $+$ text "}"
  pp (SImport fn) = text "import" <+> text fn
  pp SEof = text "eof"
  pp (EReset f) = text "reset ("$+$ nest 2 (pp f) $+$ text ")"
  pp (EShift f) = text "shift ("$+$ nest 2 (pp f) $+$ text ")"

  pp (ESpawn s) = text "spawn (" $+$ nest 2 (pp s) $$ text ")"
  pp (EDetach e) = text "detach" <+> pp e
  pp (EJoin e) = text "join" <+> pp e
  pp (SAssert msg e) = text "assert" <+> text ("\""++msg++"\"") <+> pp e 

instance Pretty Value where
  pp (VInt i)         = integer $ toInteger i
  pp (VBool True)     = text "true"
  pp (VBool False)    = text "false"
  pp (VString s)      = text $ show s
  pp (VRef _ _)       = text "ref []" -- since not in IO, cannot show the content
  pp VVoid            = text "void"
  pp (VClosure s b e) = text "closure"
  pp (VPrimFun n _)   = text $ "primfun "++n
  pp (VPrimFunIO n _) = text $ "primfun io "++n
  pp (VArr vals)      = text "[" <+> text (concatMap (\v -> pPrint v ++ ", ") vals) <+> text "]"
