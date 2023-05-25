module Main (main) where

import Glow.Mock.Lurk.Server (runServer)
import Glow.Mock.Lurk.Consensus (loadConfig)
import System.Directory
import Glow.Prelude
-- path as parameter
-- runServer :: FilePath -> IO ()
main :: IO ()
main = do
  args <- getArgs
  case args of
    (path : _ ) -> do
      lc <- loadConfig path
      runReaderT runServer lc
