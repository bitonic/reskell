module Utils (
  ndResponse,
  seeOtherN,
  getDataOr,
  getFullUri,
  
  getUser
  ) where

import State
import Config

import Happstack.Server
import Happstack.State (query)

import Data.ByteString.Char8 as B

--------------------------------------------------------------------------------
-- | Ensures that the path ends
ndResponse a = nullDir >> (ok $ toResponse a)

-- | Redirects with no message
seeOtherN uri = seeOther uri $ toResponse ""

-- | Tries to get data with rqData, if it fails it passes the error to
-- handler.
getDataOr rqData handler = getDataFn rqData >>= either handler return

getFullUri :: ServerPart String
getFullUri = do
  rq <- askRq
  return $ rqUri rq ++ (rqQuery rq)

--------------------------------------------------------------------------------

-- | Tries to get the cookie with the session id. If it can't, returns
-- nothing. Then, if the session id is present, it returns the
-- user. Otherwise, it returns nothing.
getUser :: ServerPart (Maybe User)
getUser = do
  eitherSid <- getDataFn $ lookCookieValue sessionCookieName
  case eitherSid of
    (Left _)    -> return Nothing
    (Right sid) -> query $ CheckSession $ B.pack sid