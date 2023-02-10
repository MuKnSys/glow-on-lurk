{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glow.Consensus.Lurk where


import qualified Data.Text.Lazy as T
import Glow.Prelude

import qualified Data.List as L

import qualified Data.Text as TT


import Control.Lens (makeLenses,(.~),(^.),(%~))


import qualified GHC.Generics as G
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)

import Numeric (showIntAtBase )
import Data.Char (intToDigit)

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey),lpkBS)

import Glow.Ast.Targets.Lurk as LT
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack,pack)

import qualified Data.SCargot.Repr as R

import qualified Data.SCargot.Repr.Basic as B

import Glow.Ast.Atoms 


type ParticipantId = Int

data GLType = GLNatT | GLBoolT | GLStringT | GLPFT | DigestT | GLUnitT
  deriving (Show , Read , Eq , G.Generic, Ord)

instance ToJSON GLType where
instance FromJSON GLType where

data GLValue = GLNat Int | GLBool Bool | GLString T.Text | GLPF Int | DigestOf GLValue | GLUnit deriving (Show , Read , Eq , G.Generic, Ord)

instance ToJSON GLValue where
instance FromJSON GLValue where


prettyGLValue :: GLValue -> String 
prettyGLValue = \case
  GLNat k -> show k
  GLBool k -> show k
  -- GLFloat k -> show k
  GLString s -> show s
  GLPF k -> show k L.++ "(PF)"
  DigestOf x -> "dig(" L.++ (prettyGLValue x) L.++  ")" 
  GLUnit -> "()" 



instance ToJSON ByteString where
  toJSON = toJSON . unpack
  
instance FromJSON ByteString where 
  parseJSON x = pack <$> parseJSON x
  
instance ToJSON LedgerPubKey where
instance FromJSON LedgerPubKey where

instance ToJSONKey LedgerPubKey where
  toJSONKey = toJSONKeyText ( TT.pack . unpack . lpkBS) 
  

instance FromJSONKey LedgerPubKey where
  fromJSONKey = FromJSONKeyText (LedgerPubKey . pack . TT.unpack)
  
type LurkSource = T.Text

data GLContainer = GLContainer
  { _participantsIds :: ([String])
  , _stateTransitionVerifier :: LurkSource
  , _signature :: [GLType]
  }
  deriving (Show, Read, Eq)
makeLenses ''GLContainer


data Action =
    Withdraw Int
  | Deposit Int
  | Publish GLValue
 deriving (Show , Read, G.Generic, Eq , Ord)

instance ToJSON Action where
instance FromJSON Action where


data Call = Call
  { _desiredStateId :: Int
  , _caller :: LedgerPubKey
  , _action :: Action
  }
 deriving (Show , Read, G.Generic,Eq)
makeLenses ''Call

instance ToJSON Call where
instance FromJSON Call where

renderGLValueCall :: GLValue -> R.SExpr Atom
renderGLValueCall = \case
  GLNat 0 -> B.A $ BA Nil
  GLNat k ->  B.L $ map (\case
                                              '1' -> B.A $ BA T
                                              _ -> B.A $ BA Nil)
                   (reverse $ showIntAtBase 2 intToDigit k "")
  -- GLFloat k -> B.L $ map (\case
  --                              '1' -> B.A $ BA T
  --                              _ -> B.A $ BA Nil)
  --                       (reverse $ showFloat k ""  )
               
  GLBool k -> if k then B.A $ BA T else  B.A $ BA Nil
  GLString k -> (B.A $ Str (T.unpack k))
  GLPF k -> (B.A $ Num k)
  DigestOf x -> B.L [B.A $ LK DIGEST , renderGLValueCall x]
  GLUnit -> B.A $ LK GlowUnitLitQ --lk

renderGLValue :: GLValue -> R.SExpr Atom
renderGLValue = \case
  GLNat 0 -> B.A (BA Nil)
  GLNat k ->  B.L $ (B.A $ LK Quote) : [B.L $ (map (\case
                                              '1' -> B.A $ BA T
                                              _ -> B.A $ BA Nil)
                   (reverse $ showIntAtBase 2 intToDigit k ""))]
  -- GLFloat k -> B.L $ map (\case
  --                              '1' -> B.A $ BA T
  --                              _ -> B.A $ BA Nil)
                
  --                       (reverse $ showFloat k ""  )
  GLBool k -> if k then B.A $ BA T else  B.A $ BA Nil
  GLString k -> (B.A $ Str (T.unpack k))
  GLPF k -> (B.A $ Num k)
  DigestOf x -> B.L [B.A $ LK DIGEST , renderGLValueCall x]
  GLUnit -> B.A $ LK GlowUnitLitQ

translateGLValue :: GLValue -> LT.Expr ()
translateGLValue = \case
  GLNat 0 -> LT.ExNil ()
  GLNat k -> LT.ExQuote () $ B.L $ map (\case
                               '1' -> B.A $ BA T
                               _ -> B.A $ BA Nil) 
                         (reverse $ showIntAtBase 2 intToDigit k "")

  -- GLFloat k -> LT.ExQuote () $ B.L $ map (\case
  --                               '1' -> B.A $ BA T
  --                               _ -> B.A $ BA Nil)
  --                       (reverse $ showFloat k "")

  GLBool k -> if k then (LT.ExT ()) else LT.ExNil ()
  GLString s -> (LT.ExString () $ s)
  GLPF k -> LT.ExFieldElem () k
  DigestOf x -> LT.mkConsList $ [LT.ExQuote () (B.A $ LK DIGEST), translateGLValue x]

  GLUnit -> LT.ExQuote () (B.A $ LK GlowUnitLit)





data LMState = LMState
  { _publicValues :: [GLValue]
  , _stateId :: Int
  }
 deriving (Show,G.Generic)
makeLenses ''LMState

instance ToJSON LMState where
instance FromJSON LMState where

  
initialLMState :: LMState
initialLMState = LMState
  { _publicValues = []
  , _stateId = 0
  }


data LMEnv = LMEnv
  { _identities :: [LedgerPubKey]
  , _interactionParameters :: [GLValue]
  , _stateTransitionVerifierSrc :: LurkSource
  }
 deriving (Show,G.Generic)
makeLenses ''LMEnv


instance ToJSON LMEnv where
instance FromJSON LMEnv where



data DeployedContract = DeployedContract
  { _initializationData :: LMEnv
  , _currentState :: LMState
  }
 deriving (Show, G.Generic)
makeLenses ''DeployedContract

instance ToJSON DeployedContract where
instance FromJSON DeployedContract where



  
makeStateChange :: Call -> LMState -> LMState 
makeStateChange c s = 
  let s' = (stateId .~ c ^. desiredStateId) s
  in (case c ^. action of
       Withdraw _ -> s'  
       Deposit _ -> s'
       Publish v -> (publicValues %~ (L.++[v])) s')  
