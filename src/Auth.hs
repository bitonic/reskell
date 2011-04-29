{-# Language FlexibleContexts #-}

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

import Types
import Pages.Common



-- | Calls the DB query that puts the 'Session' in the db, and put the
-- session id in a cookie.
makeSession ::
  ( MonadContext m
  , FilterMonad Response m
  , MonadError AppError m
  , MonadIO m
  , Functor m
  ) => UserName -> m ()
makeSession userName = do
  sessionId' <- liftIO genSessionId
  dbTime userUpdate $ NewSession sessionId' userName
  addCookie (MaxAge maxBound) (mkCookie sessionCookie $ B8.unpack sessionId')


-- | The opposite of makeSession, remove the 'Session' from the DB and
-- expire the cookie. If the cookie is not there, does nothing.
expireSession :: PageM ()
expireSession = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  expireCookie sessionCookie
  case eitherSid of
    Left _ -> return ()
    Right sessionid -> userUpdate $ DeleteSession (B8.pack sessionid)


-- | Function to call at the very beginning. Checks if theres a cookie
-- with a valid session id. If there is, gets the 'User' and puts it
-- in the 'Session' stored in the 'ReaderT'.
getSessionUser :: AppM a -> AppM a
getSessionUser f = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookie
  case eitherSid of
    Left _          -> local (\ctx -> ctx {sessionUser = Nothing}) f
    Right sessionid -> do
      user <- userQuery $ CheckSession (B8.pack sessionid)
      local (\ctx -> ctx {sessionUser = user}) f
  

-- | Barrier to restrict some pages to certain users. Takes an
-- authorization function, and a function that generates a response
-- given the user.
-- If the user is not there, redirects to the login page.      
-- If it's there but it has no permission, gives a 403 error.
checkUser :: (User -> Bool) -> (User -> PageM Response) -> PageM Response
checkUser checkf act = do
  userM <- askContext sessionUser
  case userM of
    Nothing -> routeRedirect R_Login >>= seeOther'
    Just user -> if checkf user
                 then act user
                 else forbiddenError
  

anyUser :: a -> Bool
anyUser _ = True