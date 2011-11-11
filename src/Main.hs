{-# Language CPP, DeriveDataTypeable #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (msum)

import Data.Acid
import Data.Acid.Local

import qualified Happstack.Server as S
import Happstack.Server hiding (Conf (..))

import System.Console.CmdArgs
import System.Log.Logger (Priority (..), logM)

import Auth
import Logger
import Routes
import Types

main :: IO ()
main = withLogger $ do
  args' <- cmdArgs cmdData
  
  let acquireDB = do
        pdb <- openPostDB $ store args'
        udb <- openUserDB $ store args'
        return (pdb, udb)

  bracket acquireDB releaseDB $ \(pdb, udb) ->

    bracket (forkIO $ runServer (static args') (port args') pdb udb) killThread $
    \_ -> do
      logM "Happstack.Server" NOTICE
           "System running, press 'e <ENTER>' or Ctrl-C to stop server"
      waitForTermination
  
  where
    releaseDB (pdb, udb) = createCheckpointAndClose pdb >> createCheckpointAndClose udb

-- Function taken from happstack-state. Right now happstack-state
-- doesn't compile and I don't want it to have as a dependency anyway,
-- so here it is.
waitForTermination :: IO ()
waitForTermination = do
#ifdef UNIX
  istty <- queryTerminal stdInput
  mv <- newEmptyMVar
  installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  case istty of
    True  -> do installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
               return ()
    False -> return ()
  takeMVar mv
#else
  let loop 'e' = return () 
      loop _   = getChar >>= loop
  loop 'c'
#endif

runServer :: FilePath -> Int -> AcidState PostDB -> AcidState UserDB -> IO ()
runServer fp port' pdb udb = do
  
  let context = Context { sessionUser = Nothing
                        , postDB      = pdb
                        , userDB      = udb
                        }

  simpleHTTP (nullConf {S.port = port'}) $ do
    -- No files
    decodeBody (defaultBodyPolicy "/tmp/" 0 10000 1024)
    
    msum [ dir "static" $ serveDirectory DisableBrowsing [] fp
         , mapServerPartT (unpackApp context) (getSessionUser runRoutes)
         ]

              
data CmdData = CmdData { static :: String
                       , store  :: String
                       , port   :: Int
                       }
             deriving (Read, Show, Data, Typeable)

cmdData :: CmdData
cmdData = CmdData { store = "_local"
                    &= typ "DIR"
                    &= help "The directory in which the state will be stored (_local)"

                  , static = "resources"
                    &= typ "DIR"
                    &= help "The directory searched for static files (resources)"

                  , port = 8000
                    &= typ "NUM"
                    &= help "Port to bind http server (8000)"
                  }
          &= help "Haskell hybrid between reddit ans hacker news."
          &= summary "reskell v0.0, (C) Francesco Mazzoli 2010"
