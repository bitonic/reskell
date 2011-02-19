module Main (
  main
  ) where

import State
import Templates
import Forms

import Control.Monad (msum)
import Control.Exception (bracket)

import Data.Monoid (mempty)

import Data.Text (Text)

import Happstack.Server
import Happstack.Server.FileServe
import Happstack.State (query, update, startSystemState, shutdownSystem, Proxy(..),
                        createCheckpoint)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive.Types  ((<++))
import Text.Digestive.Blaze.Html5 (childErrors, renderFormHtml)
import Text.Digestive.Forms.Happstack (eitherHappstackForm)


resourcesDir = "/home/astroboy/src/hsnews/resources"

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

ndResponse a = nullDir >> (ok $ toResponse a)

login :: ServerPart Response
login = do
  users <- query GetUsers
  decodeBody myPolicy
  r <- eitherHappstackForm (loginForm users <++ childErrors) "login-form"
  case r of
    Left form' -> ndResponse $ loginTemplate $
                  formTemplate form' "/users/login" "Login"
    Right (UserData username passwd) ->
      seeOther "/" $ toResponse "The login was successful, redirecting."

register :: ServerPart Response
register = do
  users <- query GetUsers
  decodeBody myPolicy
  r <- eitherHappstackForm (registerForm users <++ childErrors) "register-form"
  case r of
    Left form' -> ndResponse $ registerTemplate $
                  formTemplate form' "/users/register" "Register"
    Right (UserData username passwd) -> do
      update $ InsertUser username passwd
      ndResponse registerSuccessTemplate


usersHandlers :: ServerPart Response
usersHandlers =
  msum [ dir "register" register
       , dir "login" login
       , do nullDir
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