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
  access safe Master pool (use db q)
  

{-
wrapIO :: Access m
          => (WriteMode -> MasterOrSlaveOk -> Pipe -> IO a)
          -> m a
-- ^ Lift IO with Access context and failure into Access monad
wrapIO act = do
  writeMod <- context
  mos <- context
  pipe <- context
  liftIO $ act writeMod mos pipe

-- | This function is dangerous.
interleavedAction :: DbAccess m => ReaderT Database (Action IO) a -> m a
interleavedAction q = do
  db <- context
  wrapIO $ \w mos pipe ->
    unsafeInterleaveIO $ do 
      e <- runAction (use db q) w mos pipe
      return $ either (error . show) id e


runQuery :: NetworkIO m => ReaderT Database (Action m) a -> m (Either Failure a)
runQuery q = do
  pool <- connPool
  access safe Master pool (use database q)
-}