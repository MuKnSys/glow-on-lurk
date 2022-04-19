-- | This module defines Ast nodes that appear in more than one Ast/IR.
module Glow.Ast.Common where

import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as LT
import Glow.Prelude

-- | A variable
newtype Var = Var LT.Text
  deriving (Show, Read, Eq, Ord)

data IntType = IntType
  { itSigned :: !Bool,
    itNumBits :: !Integer
  }
  deriving (Show, Read, Eq)

data Constant
  = CBool !Bool
  | CByteString !BS.ByteString
  | CInt IntType !Integer
  deriving (Show, Read, Eq)

cInteger :: Integer -> Constant
cInteger i = CInt (intType i) i

intType :: (Bits i, Integral i) => i -> IntType
intType i =
  case bitSizeMaybe i of
    Just n -> IntType (isSigned i) (toInteger n)
    Nothing -> IntType (isSigned i) (ceil8 (1 + bitLength i))

bitLength :: (Bits i, Integral i) => i -> Integer
bitLength i =
  if i == 0 || i == -1
    then 0
    else
      let n = abs (popCount i)
       in toInteger n + bitLength (shiftR i n)

ceil8 :: Integral i => i -> i
ceil8 i =
  case rem i 8 of
    0 -> i
    r -> i + (8 - r)
