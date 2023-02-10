{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Glow.Precompiled.LMonadToLurk where


import Glow.Precompiled.LMonad
import Glow.Ast.Common
import qualified Glow.Ast.Targets.Lurk as L
import Glow.Gerbil.Types
import Prelude (error,map, ($),fromIntegral, show, IO (), putStrLn, (++))

import qualified Glow.Consensus.Lurk as C

import Data.String

import qualified Data.Text.Lazy  as LT
import Data.ByteString.Char8 (unpack)
-- import qualified Data.ByteString as BS


import Glow.Translate.LurkToSExpr(translateExpr ,sExpPrinter)
import Glow.Ast.Atoms
import Data.SCargot.Print
import qualified Data.SCargot as C
                                         
symEx :: LT.Text -> L.Expr ()
symEx x = L.ExSymbol () (L.Symbol x)

      -- cons : HExpr → HExpr → HExpr
      -- cons x x₁ = ExApply a (symEx consI) (x ∷ [ x₁ ])

appS :: LT.Text -> [L.Expr ()] -> L.Expr ()
appS f xs = L.ExApply () (symEx f ) xs

pidToLExpr :: PID -> L.Expr ()
pidToLExpr x = 
  case idBS x of
    "A" -> L.ExFieldElem () 0
    "B" -> L.ExFieldElem () 1
    _ -> error "todo"


fixGlowBuiltIns :: String -> String
fixGlowBuiltIns = \case
  "-" -> "-Nat"
  "*" -> "*Nat"
  "+" -> "+Nat"
  x -> x

tExprToLurk :: TrivExpr -> L.Expr ()
tExprToLurk = \case
  TrexVar x -> symEx (LT.pack (fixGlowBuiltIns (unpack (idBS x))))
  TrexConst x -> C.translateGLValue (constantToGLValue x)

constantToGLValue :: Constant -> C.GLValue
constantToGLValue =
  \case
     CBool t -> C.GLBool t
     CByteString x -> C.GLString (LT.pack $ unpack $ x)
     CInt _ k -> C.GLNat $ fromIntegral k
     CUnit -> C.GLUnit

    
exprToLurk :: Expr -> L.Expr ()
exprToLurk = \case
  PDigest [e] -> appS "digestprim" [tExprToLurk e] 
  PDigest _ -> error "not implemented"
  PEqlExpr x y -> appS "eq" [tExprToLurk x,tExprToLurk y]
  PAppExpr f args -> appS (LT.pack (fixGlowBuiltIns (unpack (idBS f)))) (map tExprToLurk args)
  PTrvExpr e -> tExprToLurk e

  
lMonadToLurk :: LMonad -> L.Expr ()
lMonadToLurk = \case
   Action eid pid a ->
      appS "action" [L.ExFieldElem () eid , pidToLExpr pid , h a ]
     where
       h :: Action -> L.Expr ()
       h (WithdrawA x) = appS "withdraw" [(exprToLurk x)]
       h (DepositA x) = appS "deposit" [(exprToLurk x)]
   RequireLM e -> appS "require" [exprToLurk e]
   ExpectPub eid pid -> appS "publish" [L.ExFieldElem () eid , pidToLExpr pid]
   Bind i x y ->
       appS "bind" [lMonadToLurk x
                   , L.ExLambda () [L.Symbol (LT.pack (unpack (idBS i)))] (lMonadToLurk y) ]
   Next x y -> appS "next" [lMonadToLurk x , lMonadToLurk y]
   Pure e -> appS "pure" [exprToLurk e]
   Branch e t f -> L.ExIf () (exprToLurk e) (lMonadToLurk t) (lMonadToLurk f)



mkVerifier :: [Id] -> LMonad -> String
mkVerifier ptsp m =
  LT.unpack $ LT.fromStrict $ (C.encode sExpPrinter [(L.ExLambda () (map (\i -> L.Symbol (LT.pack (unpack (idBS i)))) ptsp)(lMonadToLurk m))])

