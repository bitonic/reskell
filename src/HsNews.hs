module Main (
  main
  ) where

import Control.Monad (msum)
import Control.Monad.Trans (liftIO)
import Control.Exception (bracket)

import Text.Digestive.Types ((<++))
import Text.Digestive.Blaze.Html5 (childErrors)
import Text.Digestive.Forms.Happstack (eitherHappstackForm)

import Crypto.PasswordStore (genSaltIO)

import Happstack.Server
import Happstack.State

import Config
import State
import Auth
import Forms
import Templates
import Utils


userCP :: User -> ServerPart Response
userCP user = do
  users <- query GetUsers
  r <- eitherHappstackForm (userCPForm (userName user) users <++ childErrors)
       "usercp-form"
  case r of
    Left form -> renderTemplate $ userCPTemplate user $ formHtml form "/user/cp" "Save"
    Right (UserCPData p1 p p2) -> do
      liftIO genSaltIO >>= (update . InsertUser (userName user) p)
      ok $ toResponse "Edited correctly."


register :: ServerPart Response
register = do
  userM <- getUser
  case userM of
    Just _  -> ok $ toResponse "You are registered already."
    Nothing -> do
      users <- query GetUsers
      r <- eitherHappstackForm (registerForm users <++ childErrors) "register-form"
      case r of
        Left form' -> renderTemplate $ registerTemplate $
                      formHtml form' "/user/register" "Register"
        Right (RegisterData username passwd _) -> do
          liftIO genSaltIO >>= (update . InsertUser username passwd)
          renderTemplate registerSuccessTemplate

userHandlers :: ServerPart Response
userHandlers =
  msum [ dir "register" register
       , dir "login" login
       , dir "logout" logout
       , dir "cp" $ requireRankGetUser Member $ userCP
         
         -- Various tests...
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
handlers = do
  decodeBody appPolicy
  msum [ dir "user" userHandlers
       , renderTemplate indexTemplate
       , serveDirectory DisableBrowsing [] resourcesDir
       ]

main :: IO ()
main =
  bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
  \_ -> simpleHTTP nullConf handlers
  where
    createCheckpointAndShutdown control =
      createCheckpoint control >> shutdownSystem control