module Main (
  main
  ) where

import Config
import State
import Auth

import Control.Monad (msum)
import Control.Exception (bracket)

import Happstack.Server
import Happstack.State

usersHandlers :: ServerPart Response
usersHandlers =
  msum [ dir "register" register
       , dir "login" login
       , dir "logout" logout
       , dir "test" $ dir "member" $
         requireRank Member $ ok $ toResponse "You are a member!"
       , dir "test" $ dir "admin" $
         requireRank Admin $ ok $ toResponse "You are an admin!"
       , dir "test" $ dir "session" $
         do nullDir
            sessions <- query GetSessions
            ok $ toResponse $ show sessions
       , dir "test" $ dir "users" $
         do nullDir
            us <- query GetUsers
            ok $ toResponse $ show us
       ]

handlers :: ServerPart Response
handlers =
  msum [ dir "users" usersHandlers
       , serveDirectory DisableBrowsing [] resourcesDir
       ]

main :: IO ()
main =
  bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
  \_ -> simpleHTTP nullConf handlers
  where
    createCheckpointAndShutdown control =
      createCheckpoint control >> shutdownSystem control
