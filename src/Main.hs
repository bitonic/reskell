{-# Language DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Environment      (getProgName)
import System.Log.Logger       (Priority (..), logM)

import Control.Concurrent      (forkIO, killThread)
import Control.Exception       (bracket)
import Control.Monad           (msum)
import Control.Monad.Trans     (liftIO)

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
  
  bracket (forkIO $ runServer args' pool) killThread $ \_ -> do
    logM "Happstack.Server" NOTICE "System running, press 'e <ENTER>' or Ctrl-C to stop server"
    waitForTermination
  where
    runServer args' pool = S.simpleHTTP (S.nullConf { S.port = port args' }) $ do
      time <- liftIO $ getCurrentTime
      let context = C.Context { C.database    = M.Database (M.u $ database args')
                              , C.connPool    = pool 
                              , C.sessionUser = Nothing
                              , C.currTime    = time
                              }
      msum [ S.dir "static" $ S.serveDirectory S.DisableBrowsing [] (static args')
           , runRoutes $ context { C.currTime = time }
           ]
      

              
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
  putStrLn $ "Port: " ++ (show $ port args')
  putStrLn $ "Database: " ++ (database args') ++
    ", mongoDB host: " ++ (mongohost args') ++
    ", pool size: " ++ (show $ poolsize args')
  putStrLn $ "static files dir: " ++ (static args')