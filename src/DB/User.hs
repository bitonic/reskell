{-# Language OverloadedStrings, DeriveDataTypeable #-}

module DB.User (
    UserRank, User (..)
  , hashUserPassword, insertUser, checkLogin
  , newSession, checkSession
  ) where

import Prelude hiding (lookup)

import Control.Monad           (MonadPlus, liftM, mplus)
import Control.Monad.IO.Class

import Data.Data               (Data, Typeable)
import Data.UString
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as BS

import Database.MongoDB

import Crypto.PasswordStore

import Numeric                 (showHex)

import DB.Common




data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val UserRank where
  val     = val . show
  cast' v = liftM read $ cast' v



data User = User { userName :: UString
                 , userPassword :: UString
                 , userRank :: UserRank
                 , userAbout :: UString
                 }
          deriving (Eq, Ord, Read, Show)

instance Bson User where
  toBson user = [ "username" =: userName user 
                , "password" =: userPassword user
                , "rank"     =: userRank user
                , "about"    =: userAbout user
                ]
  fromBson doc = do
    name <- lookup "username" doc
    password <- lookup "password" doc
    rank <- lookup "rank" doc
    about <- lookup "about" doc
    return $ User name password rank about
  
  
hashStrength :: Int
hashStrength = 12


userCollection :: Collection
userCollection = "user"

sessionCollection :: Collection
sessionCollection = "session"

-------------------------------------------------------------------------------

hashUserPassword :: User -> IO User
hashUserPassword user = do
  hashedp <- makePassword (toByteString $ userPassword user) hashStrength
  return user { userPassword = fromByteString_ hashedp }

insertUser :: DbAccess m => User -> m ()
insertUser user = updateItem (Select [ "username" =: userName user ] userCollection)
                  user
  
getUser :: DbAccess m => UString -> m (Maybe User)
getUser username = getItem (select [ "username" =: username ] userCollection)

checkLogin :: DbAccess m => UString -> ByteString -> m (Maybe User)
checkLogin username password = do
  userM <- getUser username
  return $ do 
    user <- userM
    if verifyPassword password $ toByteString $ userPassword user 
      then return user 
      else Nothing


-------------------------------------------------------------------------------

newSession :: (DbAccess m, MonadIO m) => User -> m String
newSession user = do
  salt <- liftIO genSaltIO
  (Oid x y) <- liftIO genObjectId 
  let sessionid = (showHex x . (showHex y)) (BS.unpack $ exportSalt salt)
  let session = [ "_id"      =: pack sessionid
                , "username" =: userName user 
                ]
  insert_ sessionCollection session
  return sessionid


checkSession :: DbAccess m => ByteString -> m (Maybe User)
checkSession sessionid = do
  let sessionidM = fromByteString sessionid `mplus` Nothing
  case sessionidM of
    Nothing -> return Nothing
    Just sessionid' -> do
      sessionM <- findOne $ select [ "_id" =: sessionid' ] sessionCollection
      case sessionM of
        Nothing -> return Nothing
        Just session -> lookup "username" session >>= getUser