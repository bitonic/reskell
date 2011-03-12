module Main (
  main
  ) where

import Control.Exception (bracket)

import Happstack.Server
import Happstack.State

import State
import Handlers

main :: IO ()
main =
  bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
  \_ -> simpleHTTP nullConf handlers
  where
    createCheckpointAndShutdown control =
      createCheckpoint control >> shutdownSystem control