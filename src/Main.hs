{-# Language DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Log.Logger       (Priority (..), logM)

import Control.Concurrent      (forkIO, killThread)
import Control.Exception       (bracket)
import Control.Monad           (msum)

import Data.Acid

import Happstack.Server hiding (Conf (..))
import qualified Happstack.Server as S
import Happstack.State         (waitForTermination)

import Types
import Routes
import Auth
import Logger

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

runServer :: FilePath -> Int -> AcidState PostDB -> AcidState UserDB -> IO ()
runServer fp port' pdb udb = do
  
  let context = Context { sessionUser = Nothing
                        , postDB      = pdb
                        , userDB      = udb
                        }

  simpleHTTP (nullConf {S.port = port'}) $ do
    -- No files
    decodeBody (defaultBodyPolicy "/tmp/" 0 1024 1024)
    
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
