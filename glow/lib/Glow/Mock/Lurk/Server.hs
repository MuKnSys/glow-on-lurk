{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glow.Mock.Lurk.Server where

import Web.Scotty (scotty,get,post,json,text,Parsable,parseParam,ActionM,param,jsonData)

import Prelude
import Glow.Mock.Lurk.Consensus
import Glow.Consensus.Lurk
import Data.UUID
import Control.Monad.Reader
import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))
--import Data.Text as T
import Data.Text.Lazy as T (toStrict,pack)

import Control.Monad.RWS (runRWST,liftIO,put)
import qualified Control.Monad.RWS as CM
-- import System.IO

import Data.Maybe (fromMaybe)

import Data.Aeson (encodeFile,decodeFileStrict)

import System.Directory (doesFileExist)

instance Parsable UUID where
  parseParam = maybe (Left (pack "Unable to read UUID")) Right  . fromText . toStrict
  
runServer :: IO ()
runServer = scotty 3000 $ do
  get "/contractState/:cid" $ param "cid" >>= atC . getContractState >>= json

  get "/state" $ atC CM.get >>= json

  get "/clearState" $ atC (overrideConsensusState initialConsensusState) >> text "done"


  post "/override" $ jsonData >>= atC . overrideConsensusState >> text "done"

  post "/deploy" $ jsonData >>= atC . deployContract >>= json

  post "/interact/:cid" $ param "cid" >>= (\cid -> jsonData >>= atC . interactWithContract cid >> text "done")

  post "/interactEmit/:cid" $ do
    cid <- param "cid"
    body <- jsonData
    jsonData <- atC (interactWithContract cid body)
    emits <- liftIO $ createEvalFile
    let response = T.pack $ unlines $ emits
    text $ response

  get "/demo/loadJM" $ atC (put jmConsensusState) >> text "done"
  get "/demo/loadCoinFlip" $ atC (put coinFlipConsensusState) >> text "done"
  get "/demo/validCall0" $
      atC (do let c = (Call 1 (LedgerPubKey "A") (Publish (DigestOf (GLNat 7))))
              interactWithContract nil c
              CM.get)
      >>= json


  where

    consensusStatePath :: FilePath
    consensusStatePath = "/tmp/LurkMockConsensusState.json"

    -- TODO : handle non existing file!
    loadConsensusState :: IO ConsensusState
    loadConsensusState = do
                            doesFile <- doesFileExist consensusStatePath
                            if doesFile
                               then  return ()
                               else  writeFile consensusStatePath ""
                            fromMaybe initialConsensusState <$> decodeFileStrict consensusStatePath


    saveConsensusState :: ConsensusState -> IO () 
    saveConsensusState = encodeFile consensusStatePath


    consensusM :: ConsensusM Config
    consensusM = do
      c <- ask
      return c 
      
    atC :: CMS a -> ActionM a
    atC x =
        liftIO $ do
        s <- loadConsensusState
        (a , s' , _) <- runRWST x (consensusM) s
        -- putStrLn $ show s'
        saveConsensusState s'
        return a

