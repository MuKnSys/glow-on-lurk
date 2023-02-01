{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Translate lurk ASTs into s-expression form.
module Glow.Translate.LurkToSExpr where

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Glow.Ast.Targets.Lurk
import Glow.Prelude
import Glow.Ast.Atoms
import qualified Data.SCargot as C
import qualified Data.SCargot.Atom  as CA
import qualified Data.SCargot.Repr as R
import qualified Data.SCargot.Repr.Basic as B


fromAtom :: Atom -> T.Text
fromAtom (BA T) = "T"
fromAtom (BA Nil) = "NIL"
fromAtom (LK If) = "if"
fromAtom (LK Lambda) = "lambda"
fromAtom (LK LetBind) = "let"
fromAtom (LK LetRecBind) = "letrec"
fromAtom (OP (BinOperator n)) = T.pack ( n)
fromAtom (OP (UnaryOperator n)) = T.pack ( n)
fromAtom (LK (Begin)) = "begin"
fromAtom (LK (CurrentEnv)) = "current-env"
fromAtom (LK (FieldElem k)) = T.pack ( k)
fromAtom (LK (Eval)) = "eval"
fromAtom (Sym s) = T.pack s
fromAtom (Num a) = T.pack (show a)
fromAtom (Act WITHDRAW) = T.pack("withdraw")
fromAtom (Act DEPOSIT) = T.pack("deposit")
fromAtom (Act ACTION) = T.pack("action")
fromAtom (Act PUBLISH) = T.pack("publish")
fromAtom (Str a) = T.pack a
fromAtom (LK DIGEST) = T.pack ("digest")
fromAtom (LK Quote) = T.pack ("quote")
fromAtom (LK Apply) = T.pack ("apply")
fromAtom (LK RunGlow) = T.pack ("run-glow")
fromAtom (LK GlowCode) = T.pack ("glow-code")
fromAtom (LK GlowUnitLit) = T.pack ("glow-unit-lit")
fromAtom (LK GlowUnitLitQ) = T.pack ("''glow-unit-lit")
-- fromAtom (BoNum [a]) = T.pack ("quote") 

--fromAtom (KY (Apply))
fromAtom (LK (EString s)) = T.pack s



translateExpr :: Expr a -> R.SExpr Atom
translateExpr = \case
  ExT _ -> B.A (BA T)
  ExNil _ -> B.A (BA Nil)
  ExIf _ c t e ->
    (B.L [B.A (LK If), translateExpr c, translateExpr t, translateExpr e])
  ExLambda _ params body ->
    (B.L [B.A (LK Lambda),B.L (map translateSymbol params), translateExpr body])
  ExLet _ l ->
    (B.L (B.A (LK LetBind) : translateLet l ))
  ExLetRec _ l ->
    (B.L (B.A (LK LetRecBind) : translateLet l))
  ExBinary _ op l r ->
    (B.L [B.A (OP (BinOperator (translateBinOp op))), translateExpr l, translateExpr r])
  ExUnary _ op arg ->
    (B.L [B.A (OP (UnaryOperator (translateUnaryOp op))), translateExpr arg])
  ExBegin _ exs ex ->
    (B.L (B.A (LK Begin) : map translateExpr (exs <> [ex])))
  ExCurrentEnv _ ->
    (B.L [B.A (LK CurrentEnv)])
  ExFieldElem _ k ->
    (B.A (LK (FieldElem (show k))))
  ExEval _ exp Nothing ->
    (B.L [B.A (LK Eval), translateExpr exp])
  ExEval _ exp (Just env) ->
    (B.L [B.A (LK Eval), translateExpr exp, translateExpr env])
  ExSymbol _ sym ->
    translateSymbol sym 
  ExApply _ f args ->
    (B.L $ map translateExpr (f: args))
  ExQuote _ sexpr ->
    (B.L $ [B.A $ LK Quote, sexpr])
  ExString _ s -> 
    (B.A (LK (EString (show s))))

--ExQuote a (R.SExpr A.Atom) 
sExpPrinter :: C.SExprPrinter Atom (Expr a)
sExpPrinter
   = C.setFromCarrier translateExpr
   $ C.flatPrint fromAtom


sExpPrinterA
  = C.basicPrint fromAtom



  
translateSymbol :: Symbol -> R.SExpr Atom
translateSymbol (Symbol txt) =
 (B.A (Sym (LT.unpack txt)))

translateBinOp :: BinOp -> String
translateBinOp = \case
  BOpCons -> "cons"
  BOpPlus -> "+"
  BOpMinus -> "-"
  BOpTimes -> "*"
  BOpDiv -> "/"
  BOpGT -> ">"
  BOpLT -> "<"
  BOpNumEq -> "="
  BOpPtrEq -> "eq"


translateUnaryOp :: UnaryOp -> String
translateUnaryOp = \case
  UOpCar -> "car"
  UOpCdr -> "cdr"
  UOpEmit -> "emit"

translateLet :: Let a -> [B.SExpr Atom]
translateLet l =
  [ B.L $ map translateBinding (letBindings l),
    translateExpr $ letBody l
  ]

translateBinding :: Binding a -> B.SExpr Atom 
translateBinding b =
    B.L
    [ translateSymbol (bKey b),
      translateExpr (bVal b)
    ]


