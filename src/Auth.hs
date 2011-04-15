module Auth (
    makeSession
  , getSessionUser  
  , expireSession
  , checkUser
  , anyUser
  ) where


import Control.Monad.Reader    (local)

import qualified Data.ByteString.Char8 as B8

import Happstack.Server

import Web.Routes.Happstack

import Types
import DB


makeSession userName = do
  sessionid <- query $ newSession userName
  addCookie (MaxAge maxBound) (mkCookie sessionCookie sessionid)


expireSession :: PageM ()
expireSession = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  expireCookie sessionCookie
  case eitherSid of
    Left _ -> return ()
    Right sessionid -> query $ deleteSession sessionid

getSessionUser f = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  case eitherSid of
    Left _          -> local (\ctx -> ctx {sessionUser = Nothing}) f
    Right sessionid -> do
      user <- query $ checkSession sessionid
      local (\ctx -> ctx {sessionUser = user}) f
  
-- checkUser :: Route -> (User -> Bool) -> (User -> PageM Response) -> PageM Response
checkUser route checkf act = do
  userM <- askContext sessionUser
  case userM of
    Nothing -> seeOtherURL (R_Login route)
    Just user -> if checkf user
                 then act user
                 else forbiddenError
  

anyUser _ = True