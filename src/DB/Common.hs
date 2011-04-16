{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module DB.Common (
    getItem
  , query
  ) where

import Control.Monad           (liftM)
import Control.Monad.Context   (push)

import Data.Bson.Mapping

import Database.MongoDB        (DbAccess, Query, findOne, access, safe,
                                MasterOrSlaveOk (..), use, Database, Action)

import Types


getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = liftM (>>= fromBson) $ findOne q


query ::
  ( MonadContext m
  , MonadError AppError m
  , MonadIO m
  ) => ReaderT Database (Action m) b -> m b
query q = do
  Context {connPool = pool, database = db} <- getContext
  r <- access safe Master pool $ use db (push (\_ -> db) q)
  either databaseError return r  