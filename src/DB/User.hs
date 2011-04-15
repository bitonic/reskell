{-# Language OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module DB.User (
    newUser
  , getUser
  , checkLogin
  
  , newSession
  , checkSession
  , deleteSession
  ) where

import Prelude hiding (lookup)

import Control.Monad.Trans     (liftIO, MonadIO)

import Data.Bson.Mapping
import Data.ByteString         (ByteString)
import Data.Time.Clock         (getCurrentTime)

import Database.MongoDB hiding (Password)

import Crypto.PasswordStore

import Types
import DB.Common


userColl :: Collection
userColl = "user"

sessionColl :: Collection
sessionColl = "session"

hashStrenght :: Int
hashStrenght = 12

-------------------------------------------------------------------------------

newUser :: (MonadIO m, DbAccess m) => UserName -> Password -> UserRank -> String -> m ()
newUser username password rank about = do
  time <- liftIO getCurrentTime
  hpassword <- liftIO $ makePassword password hashStrenght
  let user = User { uName     = username
                  , uPassword = hpassword
                  , uRank     = rank
                  , uAbout    = about
                  , uCreated  = time
                  }
  insert_ userColl $ toBson user
  
getUser :: DbAccess m => UserName -> m (Maybe User)
getUser username = getItem $ select [$(getLabel 'uName) =: username] userColl

checkLogin :: DbAccess m => UserName -> ByteString -> m (Maybe User)
checkLogin username password = do
  userM <- getUser username
  return $ do 
    user' <- userM
    if verifyPassword password $ uPassword user' 
      then return user' 
      else Nothing


-------------------------------------------------------------------------------

newSession :: (DbAccess m, MonadIO m) => UserName -> m String
newSession userName = do
  sessionid <- liftIO genSessionId
  time <- liftIO getCurrentTime
  let session = Session sessionid userName time
  insert_ sessionColl $ toBson session
  return sessionid

  
checkSession :: DbAccess m => String -> m (Maybe User)
checkSession sessionid = do
  time <- liftIO getCurrentTime
  sessionM <- runCommand [ "findAndModify" =: sessionColl
                        , "query"         =: [$(getLabel 'sessionId) =: sessionid]
                        , "new"           =: True
                        , "update"        =: ["$set" =: [$(getLabel 'sessionTime) =: time]]
                        ]
  case (lookup "value" sessionM >>= fromBson) of
    Nothing -> return Nothing
    Just session -> getUser $ sessionUserName session


deleteSession :: DbAccess m => String -> m ()
deleteSession sessionid = delete $
                          select [$(getLabel 'sessionId) =: sessionid] sessionColl