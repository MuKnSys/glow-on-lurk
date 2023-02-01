module Glow.Ast.Atoms where

import Glow.Prelude 
import qualified Data.SCargot as C
import qualified Data.SCargot.Atom as A
import qualified Data.SCargot.Repr as R
import qualified Data.SCargot.Repr.Basic as B

data Atom = LK LurkKeyword | OP Operators | BA BooleanA | Sym String | Num Int | Act Actions | Str String   deriving (Show, Eq, Read)

data Operators = BinOperator String | UnaryOperator String deriving (Show, Eq, Read)

data Actions = WITHDRAW | DEPOSIT | ACTION | PUBLISH  deriving (Show, Eq, Read )

data BooleanA = T | Nil deriving (Show, Eq, Read)

data LurkKeyword = If | Lambda | LetBind | LetRecBind | Begin | CurrentEnv | Eval | Quote | EString String | FieldElem String | Apply | DIGEST | RunGlow | GlowCode | GlowUnitLit | GlowUnitLitQ   deriving (Show, Eq, Read)


