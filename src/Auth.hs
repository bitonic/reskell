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
logout = deleteSession `catchError` (\_ -> redir)
  where
    redir = seeOtherN "/"
    deleteSession = do
      sid <- liftM B.pack $ lookCookieValue sessionCookieName
      update $ DeleteSession sid
      expireCookie sessionCookieName
      redir

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
