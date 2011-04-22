{-# Language OverloadedStrings #-}
module DB.Common (
    getItem
  , FindAndModify (..)
  , findAndModify
  ) where


import Prelude hiding (lookup)

import Control.Monad           (liftM)
import Control.Monad.Throw

import Data.Bson.Mapping

import Database.MongoDB


-- | Generic function to get an item given a query.
getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = liftM (>>= fromBson) $ findOne q



data FindAndModify = FindAndModify { famSelector :: Selection
                                   , famSort     :: Document
                                   , famRemove   :: Bool
                                   , famUpdate   :: Modifier
                                   , famNew      :: Bool
                                   , famUpsert   :: Bool
                                   }

findAndModify :: DbAccess m => FindAndModify -> m Document
findAndModify fam = do
  doc <- runCommand [ "findAndModify" =: coll (famSelector fam)
                   , "query"        =: selector (famSelector fam)
                   , "sort"         =: famSort fam
                   , "remove"       =: famRemove fam
                   , "update"       =: famUpdate fam
                   , "new"          =: famNew fam
                   , "upsert"       =: famUpsert fam
                   ]
  case lookup "value" doc of
    Just v -> return v
    Nothing -> lookup "errmsg" doc >>= throw . QueryFailure

