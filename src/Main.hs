{-# Language DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Log.Logger       (Priority (..), logM)

import Control.Concurrent      (forkIO, killThread)
import Control.Exception       (bracket)
import Control.Monad           (msum)
import Control.Monad.Trans     (liftIO)

import Data.Time.Clock         (getCurrentTime, UTCTime)

import qualified Database.MongoDB as M

import qualified Happstack.Server as S
import Happstack.State         (waitForTermination)

import qualified Types as C
import DB
import Routes
import Auth

main :: IO ()
main = do
  args' <- cmdArgs cmdData
  
  welcomeMsg args'
  
  let db = M.Database (M.u $ database args')
  pool <- M.newConnPool (poolsize args') (M.host $ mongohost args')
  sstart <- query' pool db  $ initScoring
  
  bracket (forkIO $ runServer args' pool db sstart) killThread $ \_ -> do
    logM "Happstack.Server" NOTICE "System running, press 'e <ENTER>' or Ctrl-C to stop server"
    waitForTermination


runServer :: CmdData -> M.ConnPool M.Host -> M.Database -> UTCTime -> IO ()
runServer args' pool db sstart = do
  time <- liftIO getCurrentTime
  let context = C.Context { C.database     = db
                          , C.connPool     = pool 
                          , C.sessionUser  = Nothing
                          , C.scoringStart = sstart
                          }
  S.simpleHTTP (S.nullConf { S.port = port args' }) $ do
    S.decodeBody (S.defaultBodyPolicy "/tmp/" 4096 4096 4096)
    msum [ S.dir "static" $ S.serveDirectory S.DisableBrowsing [] (static args')
         , S.mapServerPartT (C.unpackApp context) (getSessionUser runRoutes)
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
  putStrLn $ "Port: " ++ show (port args')
  putStrLn $ "Database: " ++ database args' ++
    ", mongoDB host: " ++ mongohost args' ++
    ", pool size: " ++ show (poolsize args')
  putStrLn $ "static files dir: " ++ static args'