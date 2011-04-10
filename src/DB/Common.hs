{-# Language OverloadedStrings #-}

module DB.Common (
    getItem
  , query
  ) where

import Control.Monad           (liftM)
import Control.Monad.Trans     (lift)
import Control.Monad.Reader    (ask)

import Data.Bson.Mapping

import Database.MongoDB        (DbAccess, Query, findOne, access, safe,
                                MasterOrSlaveOk (..), use)


import Types


getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = liftM (>>= fromBson) $ findOne q


query q = do
  cx <- lift ask
  let pool = connPool cx
      db   = database cx
  r <- access safe Master pool (use db q)
  either (error . show) return r
