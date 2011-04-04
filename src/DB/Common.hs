{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB.Common (
    objid2bs
  , bs2objid
  , getItem
  , runQuery
  ) where

import Control.Monad           (liftM)
import Control.Monad.Reader    (ReaderT)

import Network.Abstract        (NetworkIO)

import Data.Bson
import Data.Bson.Mapping
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CompactString.UTF8 (fromByteString_, toByteString)
import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8, decodeUtf8)

import Numeric                 (showHex, readHex)

import Safe                    (headMay)

import Database.MongoDB

import Config


instance Val Text where
  val     = val . fromByteString_ . encodeUtf8
  cast' v = liftM (decodeUtf8 . toByteString) $ cast' v
  
instance Val ByteString where
  val     = val . Binary
  cast' b = case cast' b of
    Just (Binary bs) -> Just bs
    Nothing          -> Nothing

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
    Just doc -> return $ case fromBson doc of
      Nothing -> Nothing
      Just i  -> Just i

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
-}

runQuery :: NetworkIO m => ReaderT Database (Action m) a -> m (Either Failure a)
runQuery q = do
  pool <- connPool
  access safe Master pool (use database q)