{-# Language TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.User (
    UserRank (..)
  , User (..)
  , UserName
  , Password
  , Session (..)
  , genSessionId
  , sessionCookie
  ) where


import Control.Monad.Reader

import Data.Bson               
import Data.Bson.Mapping
import Data.Data               (Data, Typeable)
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Time.Clock         (UTCTime)


import Crypto.PasswordStore

import Numeric                 (showHex)




instance Val ByteString where
  val     = val . Binary
  cast' b = case cast' b of
    Just (Binary bs) -> Just bs
    Nothing          -> Nothing



data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)
$(deriveBson ''UserRank)

type UserName = String
type Password = ByteString

data User = User { uName     :: UserName
                 , uPassword :: Password
                 , uRank     :: UserRank
                 , uAbout    :: String
                 , uCreated  :: UTCTime
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveBson ''User)


sessionCookie :: String
sessionCookie = "session"

data Session = Session { sessionId       :: ByteString
                       , sessionUserName :: UserName
                       , sessionTime     :: UTCTime
                       }
             deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveBson ''Session)


genSessionId :: IO ByteString
genSessionId = do
  oid <- liftM oid2bs genObjectId
  salt <- liftM exportSalt genSaltIO
  return $ B8.concat [oid, salt]
  where
    oid2bs (Oid a b) = B8.pack . showHex a . showHex b $ ""