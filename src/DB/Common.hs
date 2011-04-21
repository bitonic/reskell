module DB.Common (
    getItem
  ) where

import Control.Monad           (liftM)

import Data.Bson.Mapping

import Database.MongoDB


-- | Generic function to get an item given a query.
getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = liftM (>>= fromBson) $ findOne q