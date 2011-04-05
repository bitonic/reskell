{-# Language OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module DB.User (
    newUser
  , getUser
  , checkLogin
  
  , newSession, checkSession
  ) where

import Prelude hiding (lookup)

import Control.Monad           (mplus)
import Control.Monad.IO.Class

import Data.Bson.Mapping
import Data.UString
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as BS

import Database.MongoDB hiding (Password)

import Crypto.PasswordStore

import Numeric                 (showHex)

import Types
import DB.Common


userColl :: Collection
userColl = "user"

sessionColl :: Collection
sessionColl = "session"


-------------------------------------------------------------------------------

newUser :: DbAccess m => User -> m ()
newUser user' = insert_ userColl $ toBson user'
  
getUser :: DbAccess m => UserName -> m (Maybe User)
getUser username = getItem $ select [ $(getLabel 'userName) =: username ] userColl

checkLogin :: DbAccess m => UserName -> ByteString -> m (Maybe User)
checkLogin username password = do
  userM <- getUser username
  return $ do 
    user' <- userM
    if verifyPassword password $ userPassword user' 
      then return user' 
      else Nothing


-------------------------------------------------------------------------------

newSession :: (DbAccess m, MonadIO m) => User -> m String
newSession user' = do
  salt <- liftIO genSaltIO
  Oid x y <- liftIO genObjectId 
  let sessionid = (showHex x . showHex y) (BS.unpack $ exportSalt salt)
  let session = [ "_id"      =: pack sessionid
                , "username" =: userName user' 
                ]
  insert_ sessionColl session
  return sessionid


checkSession :: DbAccess m => ByteString -> m (Maybe User)
checkSession sessionid = do
  let sessionidM = fromByteString sessionid `mplus` Nothing
  case sessionidM of
    Nothing -> return Nothing
    Just sessionid' -> do
      sessionM <- findOne $ select [ "_id" =: sessionid' ] sessionColl
      case sessionM of
        Nothing -> return Nothing
        Just session -> lookup "username" session >>= getUser