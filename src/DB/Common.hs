{-# Language OverloadedStrings #-}

module DB.Common (
    Bson (..)
  , objid2bs
  , bs2objid
  , getItem
  , updateItem
  , lazyQuery
  , runQuery
  ) where

import System.IO.Unsafe        (unsafeInterleaveIO)

import Control.Monad           (liftM)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Reader    (ReaderT)
import Control.Monad.Context
import Control.Monad.Throw     (throw)

import Network.Abstract        (NetworkIO)

import Data.Bson
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as BS

import Numeric                 (showHex, readHex)

import Safe                    (headMay)

import Database.MongoDB



import Config



class Bson a where
  toBson     :: a -> Document
  fromBson   :: DbAccess m => Document -> m a


------------------------------------------------------------------------------
-- | Convert 'ObjectId' into 'ByteString'
objid2bs :: ObjectId -> ByteString
objid2bs (Oid a b) = BS.pack . showHex a . showChar '-' . showHex b $ ""


------------------------------------------------------------------------------
-- | Convert 'ByteString' into 'ObjectId'
bs2objid :: ByteString -> Maybe ObjectId
bs2objid bs = do
  case BS.split '-' bs of
    (a':b':_) -> do
      a <- fmap fst . headMay . readHex . BS.unpack $ a'
      b <- fmap fst . headMay . readHex . BS.unpack $ b'
      return $ Oid a b
    _ -> Nothing


getItem :: (Bson i, DbAccess m) => Query -> m (Maybe i)
getItem q = do
  docM <- findOne q
  case docM of
    Nothing  -> return Nothing
    Just doc -> liftM Just $ fromBson doc 

updateItem :: (Bson i, DbAccess m) => Selection -> i -> m ()
updateItem s i = repsert s $ toBson i


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
lazyQuery :: DbAccess m => ReaderT Database (Action IO) a -> m a
lazyQuery q = do
  db <- context
  wrapIO $ \w mos pipe ->
    unsafeInterleaveIO $ do 
      e <- runAction (use db q) w mos pipe
      return $ either (error . show) id e

runQuery :: NetworkIO m => ReaderT Database (Action m) a -> m (Either Failure a)
runQuery q = do
  pool <- connPool
  access safe Master pool (use database q)