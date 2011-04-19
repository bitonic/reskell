module DB (
    module DB.Post
  , module DB.User
  , query
  , query'
  ) where


import Control.Monad.Reader    (runReaderT)

import Database.MongoDB

import Types

import DB.Post
import DB.User


query' pool db q = do
  r <- access safe Master pool $ use db q
  either (error . show) return r

query q = do
  Context {connPool = pool, database = db} <- getContext
  ctx <- getContext
  r <- access safe Master pool $ use db (runReaderT q ctx)
  either databaseError return r