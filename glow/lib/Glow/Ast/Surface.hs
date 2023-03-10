{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | AST fo  Glow's surface syntax.
module Glow.Ast.Surface where

import qualified Data.Text.Lazy as LT
import Glow.Prelude
import Numeric.Natural (Natural)
import qualified Glow.Translate.LurkToSExpr as L

type Str = LT.Text

type Program = [Stmt]

-- | A glow statement
data Stmt
  = StExpr Expr
  | StLet Symbol Expr
  deriving (Show, Read, Eq)

-- | A glow expression
data Expr
  = ExUnit
  | ExIdent Symbol
  | ExLiteral Literal
  | -- | ExUnary UnaryOp Expr
    ExBinary BinOp Expr Expr
  | ExLambda Function
  | ExRecord [(Symbol, Expr)]
  | ExBody [Stmt] Expr -- { x; y; z }
  deriving (Show, Read, Eq)

{-
data UnaryOp
    deriving(Show, Read, Eq)
-}

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpGT
  | BinOpLT
  | BinOpMod
  deriving (Show, Read, Eq)

data Function = Function
  { fParams :: [Param],
    fBody :: Expr
  }
  deriving (Show, Read, Eq)

data Param = Param
  { pName :: Symbol,
    pType :: Maybe Type
  }
  deriving (Show, Read, Eq)

data Type
  = TyIdent Symbol
  deriving (Show, Read, Eq)

data Literal
  = LitBool !Bool
  | LitNat !Natural
  | LitStr LT.Text
  | LitFn Function
  deriving (Show, Read, Eq)

newtype Symbol = Symbol LT.Text
  deriving (Show, Read, Eq, Ord, IsString)
