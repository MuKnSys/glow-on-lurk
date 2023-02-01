module Glow.MainLurk (main) where

import qualified Glow.Gerbil.ImportSExpr as ISExp
import Glow.Gerbil.ParseProject (extractPrograms)
import Glow.Ast.Common
import Glow.Prelude
import Text.Show.Pretty (pPrint)
import Glow.Precompiled.Base

import Glow.Runtime.Interaction.Sim

import Glow.Consensus.Lurk

import Glow.Mock.Lurk.Consensus (createEvalFile)

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))

import Glow.Runtime.Lurk.Commands
import Glow.Runtime.Interaction.InteractWithServer
import qualified Glow.Runtime.Interaction.InteractWithServerOneStep as OS
import Glow.Runtime.Interaction.Interact
import Data.UUID as UUID
import Data.Maybe (fromJust)
import Prelude
import Control.Lens 
import Data.ByteString.Char8 (unpack,pack)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("deploy" : file : _) -> do
       putStrLn $ show file
       pc <- precomp file
       putStrLn $ "Precompiled File: " ++ show pc 
       params <- map snd <$> parametersPrompt (paramsWithTypes pc)
       ptcps <- participantsPrompt (Id <$> pc ^. pcParticipantNames)
       putStrLn $ show ptcps
       putStrLn $ show params
       
       r <- deployContract pc params ptcps
       putStrLn $ "Deployed Contract : " ++ show r
       case r of
         Left err -> error err
         Right cid -> putStrLn (UUID.toString cid)
    ("interact" : file : _) -> do
       pc <- precomp file
       putStrLn "enter contract address:"      
       cid <- getLine
       putStrLn "enter privKey:"      
       pubK <- getLine
       putStrLn "enter your role:"
       role <- getLine
       params <- map snd <$> parametersPrompt (paramsWithTypes pc)
       void $ runInteractionWithServer
                (LocalInteractEnv
                 pc
                 (LedgerPubKey (pack pubK))
                 (pack role)
                 params) (fromJust $ UUID.fromString cid) 
    ("interact-cli" : file : cid : pubK : role : paramsS : i : _) -> do 
        pc <- precomp file
        let params = read paramsS
        void $ OS.runInteractionWithServer
                (LocalInteractEnv
                 pc
                 (LedgerPubKey (pack pubK))
                 (pack role)
                 params) (fromJust $ UUID.fromString cid) i

    ("interact-cli-deb" : file : cid : pubK : role : paramsS : i : _) -> do 
        pc <- precomp file
        let params = read paramsS
        emit <- createEvalFile
        putStrLn $ "Emits" ++  show emit
        void $ OS.runInteractionWithServer
                (LocalInteractEnv
                 pc
                 (LedgerPubKey (pack pubK))
                 (pack role)
                 params) (fromJust $ UUID.fromString cid) i

    ("deploy-cli" : file : paramsS : ptcpsS : _ ) -> do
        pc <- precomp file
        let params = read paramsS
        let ptcps = read ptcpsS
        r <- deployContract pc params ptcps
        case r of
          Left err -> error err
          Right cid -> putStrLn (UUID.toString cid)

    _ -> putStrLn "unrecognized args"


 where
   
   exe = "glow"

   precomp :: String -> IO PrecompiledContract
   precomp file = do
     fed <-
        ISExp.frontEndData
          ISExp.FrontEndParams
            { ISExp.fepExePath = exe,
              ISExp.fepFile = file
            }
--     do
      -- putStrLn $ "Fed variable used in precomp: " ++ show fed
     case fed of
       Left e -> error (ISExp.formatError e)
       Right v ->case  (precompile $ v) of
                       Left err -> error err
                       Right v' -> return v'
   
  -- case args of
  --   [file] -> do
  --     fed <-
  --       ISExp.frontEndData
  --         ISExp.FrontEndParams
  --           { ISExp.fepExePath = exe,
  --             ISExp.fepFile = file
  --           }
  --     case fed of
  --       Left e -> putStrLn (ISExp.formatError e)
  --       Right v -> do
  --         pPrint v
  --         pPrint (extractPrograms (ISExp.fedProject v))
  --         pPrint (ISExp.fedAnf v)
  --         pPrint (ISExp.fedTypeTable v)
  --         pPrint (precompile $ v)
  --   _ -> do
  --     putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
  --     exitFailure

