module Auth where

import qualified Data.ByteString.Char8 as B8

import Happstack.Server

import Web.Routes.Happstack

import Types
import DB


makeSession userName = do
  sessionid <- query $ newSession userName
  addCookie (MaxAge maxBound) (mkCookie sessionCookie $ B8.unpack sessionid)
  
checkUser :: Route -> (User -> Bool) -> PageM Response -> PageM Response
checkUser route checkf act = do
  userM <- askContext sessionUser
  case userM of
    Nothing -> seeOtherURL (R_Login route)
    Just user -> if checkf user
                 then act
                 else forbiddenError