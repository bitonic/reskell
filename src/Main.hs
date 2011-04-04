{-# Language DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Environment      (getProgName)
import System.Log.Logger       (Priority (..), logM)

import Control.Concurrent      (forkIO, killThread)
import Control.Exception       (bracket)

import qualified Database.MongoDB as M

import qualified Happstack.Server as S
import Happstack.State         (waitForTermination)

import qualified Config as C
import Pages


main :: IO ()
main = do
  progName <- getProgName
  
  args <- cmdArgs cmdData
  
  pool <- M.newConnPool 1 (M.host $ read $ mongohost args)
  
  let config = C.Config { C.httpConf = S.nullConf { S.port = port args }
                        , C.static   = static args
                        , C.database = M.Database (M.u $ database args)
                        , C.connPool = pool
                        }
  
  bracket (forkIO $ runServer config) killThread $ \_ -> do
              logM "Happstack.Server" NOTICE "System running, press 'e <ENTER>' or Ctrl-C to stop server"
              waitForTermination
  where
    runServer config = S.simpleHTTP' (C.unpackConfigT config) (C.httpConf config) routes

              
data CmdData = CmdData { port       :: Int
                       , database   :: String 
                       , poolsize   :: Int
                       , mongohost  :: String
                       , static     :: String
                       }
             deriving (Read, Show, Data, Typeable)

cmdData = CmdData { port = 8000 &= name "p"  &= typ "NUM" &= help "Port to bind http server (8000)"
                  , database = "reskell" &= help "The MongoDB database to be used (reskell)"
                  , mongohost = "127.0.0.1" &= help "MongoDB host (127.0.0.1)"
                  , poolsize = 1 &= name "c"  &= typ "NUM" &= help "Number of connections in the MongoDB connections pool (1)"
                  , static = "resources" &= typ "DIR" &= help "The directory searched for static files (resources)"
                  } &=
          help "Haskell hybrid between reddit ans hacker news." &=
          summary "reskell v0.0, (C) Francesco Mazzoli 2010"        

{-
defaultConnPool :: NetworkIO m => m (ConnPool Host)
defaultConnPool = newConnPool 1 (host "127.0.0.1")
-}