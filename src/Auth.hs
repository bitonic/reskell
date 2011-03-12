module Auth (
  logout, login, requireRank, requireRankGetUser
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import Happstack.Server
import Happstack.State (query, update)

import qualified Data.ByteString.Char8 as B

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
  rq <- getDataFn $ lookCookieValue sessionCookieName
  case rq of
    (Left _)    -> ok $ toResponse "You are already logged in."
    (Right sid) -> do
      update $ DeleteSession $ B.pack sid
      expireCookie sessionCookieName
      renderTemplate logoutTemplate

-- | The login page. It uses the form from Forms, and tries to get a
-- "redir" parameter which tells where to go after the login. If it
-- can't find it, it redirects to the root.
login :: ServerPart Response
login = do
  users <- query GetUsers
  r <- eitherHappstackForm (loginForm users <++ childErrors) "login-form"
  case r of
    Left form' -> do
      q <- liftM rqQuery askRq
      renderTemplate $ loginTemplate $ formHtml form' ("/user/login" ++ q) "Login"
    Right (LoginData u _) -> do
      sid <- liftIO genSaltIO >>= (update . InsertSession u)
      addCookie Session $ mkCookie sessionCookieName sid
      redir <- getDataOr (look "redir") (\_ -> return "/")
      seeOtherN redir

-- | Accepts a minimum rank and a (User -> ServerPart Response). If
-- the user is not logged in, displays the login page passing the
-- requested Path, so that the user will be redirected there after the
-- login.  If the user is logged in and his rank is less than the one
-- required, it displays a 403 error.
-- Otherwise, it calls the function that generates the response.
requireRankGetUser :: UserRank -> (User -> ServerPart Response) -> ServerPart Response
requireRankGetUser rank response = do
  userM <- getUser
  case userM of
    Just user -> checkRank user
    Nothing   -> do
      redir <- getFullUri
      seeOtherN $ "/user/login?redir=" ++ redir
  where
    checkRank user =
      if userRank user < rank
        then forbidden $ toResponse "Access denied."
        else response user

-- | Same as requireRankGetUser, but no user.
requireRank :: UserRank -> ServerPart Response -> ServerPart Response
requireRank rank response = requireRankGetUser rank (\_ -> response)