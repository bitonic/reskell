{-# Language FlexibleContexts #-}

module Auth
    ( -- * Session
      makeSession
    , getSessionUser  
    , expireSession
         
    -- * Authorization
    , checkUser
    , anyUser
    , editPost
    , deletePost
    ) where


import Control.Monad.Error (MonadError)
import Control.Monad.Reader (local)
import Control.Monad.Trans (liftIO, MonadIO)

import Data.Int (Int32)
import qualified Data.ByteString.Char8 as B8

import Happstack.Server

import Pages.Common
import Types hiding (Session)


-- | Calls the DB query that puts the 'Session' in the db, and put the
-- session id in a cookie.
makeSession ::
  ( MonadContext m
  , FilterMonad Response m
  , MonadError AppError m
  , MonadIO m
  , Functor m
  ) => UserName -> Bool -> m ()
makeSession userName permanent = do
  sessionId' <- liftIO genSessionId
  dbTime userUpdate $ NewSession sessionId' userName
  let duration = if permanent
                   then MaxAge (fromIntegral (maxBound :: Int32))
                   else Session
  addCookie duration (mkCookie sessionCookie $ B8.unpack sessionId')


-- | The opposite of makeSession, remove the 'Session' from the DB and
-- expire the cookie. If the cookie is not there, does nothing.
expireSession :: PageM ()
expireSession = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  expireCookie sessionCookie
  case eitherSid of
    Left _ -> return ()
    Right sessionid -> userUpdate $ DeleteSession (B8.pack sessionid)


{-|

Function to call at the very beginning.

Checks if theres a cookie with a valid session id. If there is, gets
the 'User' and puts it in the 'Session' stored in the 'ReaderT'
containing the 'Context'.

-}
getSessionUser :: AppM a -> AppM a
getSessionUser f = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  case eitherSid of
    Left _          -> local (\ctx -> ctx {sessionUser = Nothing}) f
    Right sessionid -> do
      user <- userQuery $ CheckSession (B8.pack sessionid)
      local (\ctx -> ctx {sessionUser = user}) f
  

{-|

Barrier to restrict some pages to certain users.

If the user is not there, redirects to the login page.

If it's there but it has no permission, gives a 403 error - see
'AppError'.

-}
checkUser :: (User -> Bool)
             -- ^ Verifies is the user is authorized.
             -> (User -> PageM Response)
             -- ^ Action to execute with the user, after it has been
             -- authorized. Won't be executed if the user is not
             -- authorizes or if there is no user in the 'Context'.
             -> PageM Response
checkUser checkf act = do
  userM <- askContext sessionUser
  case userM of
    Nothing -> routeRedirect R_Login >>= seeOther'
    Just user -> if checkf user
                 then act user
                 else forbiddenError
  

anyUser :: a -> Bool
anyUser _ = True

editPost :: Post a => a -> User -> Bool
editPost post user = case uRank user of
  Admin     -> True
  Moderator -> True
  Member    -> pUserName post == uName user

deletePost :: User -> Bool
deletePost user = case uRank user of
  Member -> False
  _      -> True
