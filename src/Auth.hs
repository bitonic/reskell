module Auth (
  logout, login, register, requireRank
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (catchError)

import Happstack.Server
import Happstack.Server.SURI (ToSURI, render)
import Happstack.State (query, update)

import qualified Data.ByteString.Char8 as B

import qualified Data.HashMap as M
import qualified Data.HashSet as S

import Text.Digestive.Types  ((<++))
import Text.Digestive.Blaze.Html5 (childErrors)
import Text.Digestive.Forms.Happstack (eitherHappstackForm)

import Crypto.PasswordStore (genSaltIO)

import Config
import Forms
import State
import Utils
import Templates

logout :: ServerPart Response
logout = do
  decodeBody appPolicy
  sid <- getDataFn $ lookCookieValue sessionCookieName
  case sid of
    (Left _)    -> ok $ toResponse "You are already logged in."
    (Right sid) -> do
      update $ DeleteSession $ B.pack sid
      expireCookie sessionCookieName
      ok $ toResponse "You are now logged out."

register :: ServerPart Response
register = do
  users <- query GetUsers
  decodeBody appPolicy
  r <- eitherHappstackForm (registerForm users <++ childErrors) "register-form"
  case r of
    Left form' -> ndResponse $ registerTemplate $
                  formTemplate form' "/users/register" "Register"
    Right (UserData username passwd) -> do
      liftIO genSaltIO >>= (update . InsertUser username passwd)
      ndResponse registerSuccessTemplate

-- | The login page. It uses the form from Forms, and tries to get a
-- "redirect" parameter which tells where to go after the login. If it
-- can't find it, it redirects to the root.
login :: ServerPart Response
login = do
  users <- query GetUsers
  decodeBody appPolicy
  r <- eitherHappstackForm (loginForm users <++ childErrors) "login-form"
  case r of
    Left form' -> do
      query <- liftM rqQuery askRq
      ndResponse $ loginTemplate $ formTemplate form' ("/users/login" ++ query) "Login"
    Right (UserData u _) -> do
      sid <- liftIO genSaltIO >>= (update . InsertSession u)
      addCookie Session $ mkCookie sessionCookieName sid
      redirect <- getDataOr (look "redirect") (\_ -> return "/")
      seeOtherN redirect

-- | Accepts a minimum rank and a ServerPart Response. If the user is
-- not logged in, displays the login page passing the requested Path,
-- so that the user will be redirected there after the login.  If the
-- user is logged in and his rank is less than the one required, it
-- displays a 403 error.
requireRank :: UserRank -> ServerPart Response -> ServerPart Response
requireRank rank response = decodeBody appPolicy >> checkSession
  where
    checkSession = do
      sid <- liftM B.pack $
             getDataOr (lookCookieValue sessionCookieName) (\_ -> return "")
      userM <- query $ CheckSession sid
      case userM of
        Just user -> checkRank user
        Nothing   -> displayLogin
    checkRank user = do
      if userRank user < rank
        then forbidden $ toResponse "Access denied."
        else response
    displayLogin = do
      redirect <- liftM (render . rqUri) askRq
      seeOtherN ("/users/login?redirect=" ++ redirect)
