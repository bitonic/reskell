module DB (
    module DB.Post
  , module DB.User
  , query
  , query'
  ) where


import Control.Monad.Context   (push)

import Database.MongoDB

import Types

import DB.Post
import DB.User
import DB.Common


query' pool db q = do
  r <- access safe Master pool $ use db q
  either (error . show) return r

query q = do
  Context {connPool = pool, database = db} <- getContext
  r <- access safe Master pool $ use db q
  either databaseError return r