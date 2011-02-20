module Main (
  main
  ) where

import State
import qualified State.Users as U
import Templates
import Forms

import Control.Monad (msum)
import Control.Monad.Trans (liftIO)
import Control.Exception (bracket)

import Happstack.Server
import Happstack.State (query, update, startSystemState, shutdownSystem, Proxy(..),
                        createCheckpoint)

import Text.Digestive.Types  ((<++))
import Text.Digestive.Blaze.Html5 (childErrors)
import Text.Digestive.Forms.Happstack (eitherHappstackForm)

import Crypto.PasswordStore (genSaltIO)

resourcesDir :: FilePath
resourcesDir = "/home/astroboy/src/hsnews/resources"

sessionCookieName :: String
sessionCookieName = "hsnews-session"

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

ndResponse a = nullDir >> (ok $ toResponse a)

logout :: ServerPart Response
logout = do
  expireCookie sessionCookieName
  seeOther "/" $ toResponse "The logout was successful, redirecting."
  
login :: ServerPart Response
login = do
  users <- query U.GetUsers
  decodeBody myPolicy
  r <- eitherHappstackForm (loginForm users <++ childErrors) "login-form"
  case r of
    Left form' -> ndResponse $ loginTemplate $
                  formTemplate form' "/users/login" "Login"
    Right (UserData u _) -> do
      sid <- liftIO genSaltIO >>= (update . U.InsertSession u)
      addCookie Session $ mkCookie sessionCookieName sid
      seeOther "/" $ toResponse "The login was successful, redirecting."

register :: ServerPart Response
register = do
  users <- query U.GetUsers
  decodeBody myPolicy
  r <- eitherHappstackForm (registerForm users <++ childErrors) "register-form"
  case r of
    Left form' -> ndResponse $ registerTemplate $
                  formTemplate form' "/users/register" "Register"
    Right (UserData username passwd) -> do
      liftIO genSaltIO >>= (update . U.InsertUser username passwd)
      ndResponse registerSuccessTemplate


usersHandlers :: ServerPart Response
usersHandlers =
  msum [ dir "register" register
       , dir "login" login
       , dir "logout" logout
       , do nullDir
            us <- query U.GetUsers
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