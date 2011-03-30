{-# Language OverloadedStrings #-}

module DB.Common (
    Bson (..)
  , showOid
  , getItem
  , updateItem
  , runQuery
  ) where

import Control.Monad           (liftM)
import Control.Monad.IO.Class  (MonadIO)
import Control.Monad.Reader    (ReaderT)

import Data.Bson

import Database.MongoDB



class Bson a where
  toBson   :: DbAccess m => a -> m Document
  fromBson :: DbAccess m => Document -> m a


showOid :: ObjectId -> String
showOid (Oid x y) = show x ++ "-" ++ (show y)

readOid :: String -> ObjectId
readOid s = Oid (read x) (read $ tail y)
  where (x, y) = break (== '-') s

getItem :: (Bson a, DbAccess m) => Query -> m (Maybe a)
getItem q = do
  docM <- findOne q
  case docM of
    Nothing    -> return Nothing
    (Just doc) -> liftM Just $ fromBson doc 

updateItem :: (Bson a, DbAccess m) => Selection -> a -> m ()
updateItem s i = toBson i >>= repsert s


database :: Database
database = Database "hsnews"

runQuery :: (MonadIO m, Service s) =>
            ConnPool s -> ReaderT Database (Action m) a -> m (Either Failure a)
runQuery pool q = access safe Master pool $ use database q