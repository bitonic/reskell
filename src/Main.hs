{-# Language DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Environment      (getProgName)
import System.Log.Logger       (Priority (..), logM)

import Control.Concurrent      (forkIO, killThread)
import Control.Exception       (bracket, onException)
import Control.Monad           (msum)
import Control.Monad.Trans     (liftIO)
import Control.Concurrent.MVar

import Data.Time.Clock         (getCurrentTime)

import qualified Database.MongoDB as M

import qualified Happstack.Server as S
import Happstack.State         (waitForTermination)

import qualified Types as C
import Routes


main :: IO ()
main = do
  progName <- getProgName
  
  args' <- cmdArgs cmdData
  
  welcomeMsg args'
  
  pool <- M.newConnPool 1 (M.host $ mongohost args')
  
  -- Prepare the various mvars
  userMVar <- newEmptyMVar
  
  bracket (forkIO $ runServer args' pool userMVar) killThread $ \_ -> do
    logM "Happstack.Server" NOTICE "System running, press 'e <ENTER>' or Ctrl-C to stop server"
    waitForTermination


runServer :: CmdData -> M.ConnPool M.Host -> MVar () -> IO ()
runServer args' pool userMVar = do
  time <- liftIO getCurrentTime
  let context = C.Context { C.database    = M.Database (M.u $ database args')
                          , C.connPool    = pool 
                          , C.sessionUser = Nothing
                          , C.currTime    = time
                                            
                          , C.userMVar    = userMVar
                          }
      http    = S.simpleHTTP (S.nullConf { S.port = port args' }) $ do
        S.decodeBody (S.defaultBodyPolicy "/tmp/" 4096 4096 4096)
        msum [ S.dir "static" $ S.serveDirectory S.DisableBrowsing [] (static args')
             , S.mapServerPartT (C.unpackApp $ context {C.currTime = time}) runRoutes
             ]
  onException http $ do
    tryPutMVar userMVar ()

              
data CmdData = CmdData { port       :: Int
                       , database   :: String 
                       , poolsize   :: Int
                       , mongohost  :: String
                       , static     :: String
                       }
             deriving (Read, Show, Data, Typeable)

cmdData :: CmdData
cmdData = CmdData { port = 8000 &= name "p"  &= typ "NUM" &= help "Port to bind http server (8000)"
                  , database = "reskell" &= help "The MongoDB database to be used (reskell)"
                  , mongohost = "127.0.0.1" &= help "MongoDB host (127.0.0.1)"
                  , poolsize = 1 &= name "c"  &= typ "NUM" &= help "Number of connections in the MongoDB connections pool (1)"
                  , static = "resources" &= typ "DIR" &= help "The directory searched for static files (resources)"
                  } &=
          help "Haskell hybrid between reddit ans hacker news." &=
          summary "reskell v0.0, (C) Francesco Mazzoli 2010"        

welcomeMsg :: CmdData -> IO ()
welcomeMsg args' = do
  putStrLn "reskell starting..."
  putStrLn $ "Port: " ++ show (port args')
  putStrLn $ "Database: " ++ database args' ++
    ", mongoDB host: " ++ mongohost args' ++
    ", pool size: " ++ show (poolsize args')
  putStrLn $ "static files dir: " ++ static args'