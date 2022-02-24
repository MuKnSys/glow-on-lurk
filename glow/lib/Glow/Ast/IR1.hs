-- | This module defines an AST for Glow's first intermediate
-- representation this side of the fronted. We convert the data types
-- in Glow.Gerbil.Types to this form before further translation.
--
-- Some salient properties:
--
-- * Everything is in administrative normal form (ANF).
-- * There are no lambdas.
--   * Closures are constructed via explicit capture operation
--     ('ExCapture'), which takes a variable denoting a function's
--     _code_ and an explicit capture list.
--   * There are separate types for raw function pointers and
--     function closures ('TFPtr' vs. 'TFunc')
--   * Rationale: for machine-like targets, this helps with memory
--     representation, and for higher-level targets, it helps with
--     serializing continuations.
-- * Effectful operations are explicit, and have different control
--   flow than effect-free operations (helps with inserting transaction
--   boundaries). Functions which have side-effects are not type compatible
--   with those that do not.
module Glow.Ast.IR1 where

import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import Glow.Prelude

-- | A Glow module. A module is a mapping of variablers to definitions,
-- along with a mapping of expected types for things defined outside
-- of this module.
data Module = Module
  { -- | Map of function names to definitions for this module:
    modFunDefs :: M.Map Var FunDef,
    -- | Types of externally defined functions/data:
    modExterns :: M.Map Var Type
  }
  deriving (Show, Read, Eq)

-- | A variable
newtype Var = Var LT.Text
  deriving (Show, Read, Eq, Ord)

-- | A function definition. In addition to the parameters,
-- return type, and body that exist in the surface language,
-- this also has an explicit capture list.
data FunDef = FunDef
  { -- | Captured variables:
    fdCaptures :: [Param],
    -- | Function parameters:
    fdParams :: [Param],
    -- | Return type:
    fdRetType :: Type,
    -- | Function body:
    fdBody :: [Stmt]
  }
  deriving (Show, Read, Eq)

-- | A type.
data Type
  = -- | The type of a function pointer:
    TFPtr FPtrType
  | -- | The type of a function/closure:
    TFunc FuncType
  | TTuple [Type]
  deriving (Show, Read, Eq)

-- | A type for a "function pointer," named by analagy to C; these are
-- references to code, and cannot be constructed at runtime, but can
-- be used to construct closures using 'ExCapture'.
data FPtrType = FPtrType
  { fptCaptures :: [Type],
    fptFuncType :: FuncType
  }
  deriving (Show, Read, Eq)

data FuncType = FuncType
  { ftParams :: [Type],
    ftResult :: Type,
    -- | Can this function have side-effects?
    ftEffectful :: !Bool
  }
  deriving (Show, Read, Eq)

data Stmt
  = STLet Let
  | StEffect Effect Var -- Effect, continuation.
  | StReturn Expr
  deriving (Show, Read, Eq)

data Asset = Asset
  { assetRef :: Var,
    assetAmount :: Var
  }
  deriving (Show, Read, Eq)

newtype Participant = Participant Var
  deriving (Show, Read, Eq)

data Effect
  = EffDeposit [Asset]
  | EffWithdraw [(Asset, Participant)]
  deriving (Show, Read, Eq)

data Let = Let
  { lVar :: Var,
    lDef :: Expr,
    lBody :: Stmt
  }
  deriving (Show, Read, Eq)

data Expr
  = ExTuple [Var]
  | ExCapture Var [Var]
  | ExCall Var [Var]
  deriving (Show, Read, Eq)

data Param = Param
  { pVar :: Var,
    pType :: Type
  }
  deriving (Show, Read, Eq)
