{-# Language OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module DB.User (
    UserRank (..)
  , User (..)
  , UserName
  , Password
  
  , hashUserPassword
  , newUser
  , getUser
  , checkLogin
  
  , newSession, checkSession
  ) where

import Prelude hiding (lookup)

import Control.Monad           (MonadPlus, liftM, mplus)
import Control.Monad.IO.Class

import Data.Data               (Data, Typeable)
import Data.Bson.Mapping
import Data.UString
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text               (Text)

import Database.MongoDB hiding (Password)

import Crypto.PasswordStore

import Numeric                 (showHex)

import DB.Common




data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val UserRank where
  val     = val . show
  cast' v = liftM read $ cast' v


type UserName = Text
type Password = ByteString

data User = User { userName :: UserName
                 , userPassword :: Password
                 , userRank :: UserRank
                 , userAbout :: Text
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveBson ''User)
  

userColl :: Collection
userColl = "user"

hashStrength :: Int
hashStrength = 12

sessionColl :: Collection
sessionColl = "session"


-------------------------------------------------------------------------------

hashUserPassword :: User -> IO User
hashUserPassword user = do
  hashedp <- makePassword (userPassword user) hashStrength
  return user { userPassword = hashedp }

newUser :: DbAccess m => User -> m ()
newUser user = insert_ userColl $ toBson user
  
getUser :: DbAccess m => UserName -> m (Maybe User)
getUser username = getItem $ select [ $(getLabel 'userName) =: username ] userColl

checkLogin :: DbAccess m => UserName -> ByteString -> m (Maybe User)
checkLogin username password = do
  userM <- getUser username
  return $ do 
    user <- userM
    if verifyPassword password $ userPassword user 
      then return user 
      else Nothing


-------------------------------------------------------------------------------

newSession :: (DbAccess m, MonadIO m) => User -> m String
newSession user = do
  salt <- liftIO genSaltIO
  Oid x y <- liftIO genObjectId 
  let sessionid = (showHex x . (showHex y)) (BS.unpack $ exportSalt salt)
  let session = [ "_id"      =: pack sessionid
                , "username" =: userName user 
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