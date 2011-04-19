module DB.Common (
    getItem
  ) where

import Control.Monad           (liftM)

import Data.Bson.Mapping

import Database.MongoDB


getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = liftM (>>= fromBson) $ findOne q