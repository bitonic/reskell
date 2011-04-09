{-# Language DeriveDataTypeable, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
  -- * Context
    Context (..)
  , ContextM
  , unpackContext
  , MonadContext (..)
  , askContext
    
  -- * User  
  , UserRank (..)
  , User (..)
  , UserName
  , Password
  , hashUserPassword
  , hashStrength
    
  -- * Posts  
  , PostId
  , SContent (..)
  , getDomain
  , Submission (..)
  , Comment (..)
  , Post (..)
    
  -- * Utilities  
  , readM
  ) where



import Control.Monad.Reader

import Data.Bson               
import Data.Bson.Mapping
import Data.Data               (Data, Typeable)
import Data.Text               (Text)
import qualified Data.Text as T
import Data.Text.Encoding      (encodeUtf8, decodeUtf8)
import Data.ByteString         (ByteString)
import Data.Time.Clock         (UTCTime)
import Data.CompactString.UTF8 (fromByteString_, toByteString)

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator

import Happstack.Server        (ServerPartT, UnWebT)

import Database.MongoDB        (Database (..), ConnPool, Host)

import Web.Routes              (RouteT)

import HSP                     (XMLGenT)

import Crypto.PasswordStore


readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse."
  where
    res = reads s


-------------------------------------------------------------------------------

instance Val Text where
  val     = val . fromByteString_ . encodeUtf8
  cast' v = liftM (decodeUtf8 . toByteString) $ cast' v
  
instance Val ByteString where
  val     = val . Binary
  cast' b = case cast' b of
    Just (Binary bs) -> Just bs
    Nothing          -> Nothing


-------------------------------------------------------------------------------
  
data UserRank = Member | Admin
              deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val UserRank where
  val     = val . show
  cast' v = cast' v >>= readM


type UserName = Text
type Password = ByteString

data User = User { uName :: UserName
                 , uPassword :: Password
                 , uRank :: UserRank
                 , uAbout :: Text
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveBson ''User)

hashStrength :: Int
hashStrength = 12

hashUserPassword :: User -> IO User
hashUserPassword user = do
  hashedp <- makePassword (uPassword user) hashStrength
  return user { uPassword = hashedp }


-------------------------------------------------------------------------------

type PostId = Int

class (Bson a, Typeable a) => Post a where
  pId       :: a -> PostId
  pTime     :: a -> UTCTime
  pUserName :: a -> UserName
  pVotes    :: a -> Int
  

data SContent = Ask Text  
              | Link Text Text -- ^ url, domain
              deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveBson ''SContent)


parseProtocol = string "http://" <|> string "https://"

parseDomain = do
  parseProtocol
  d1 <- liftM ('.' :) domainLetters
  d2 <- domainSeg
  rest <- manyTill domainSeg (eof <|> (char '/' >> return ()))
  let all' = d1 : d2 : rest
  return $ tail $ concat $ drop (length all' - 2) $ all'
  where
    domainSeg = do
      char '.'
      d <- domainLetters
      return $ '.' : d
    domainLetters = many1 $ oneOf $ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getDomain :: Text -> Maybe Text
getDomain url = case parse parseDomain "" (T.unpack url) of
  Right p -> Just $ T.pack p
  Left _  -> Nothing


data Submission = Submission { sId       :: PostId
                             , sUserName :: UserName
                             , sTime     :: UTCTime
                             , sTitle    :: Text
                             , sContent  :: SContent
                             , sVotes    :: Int
                             }
                deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Submission)

  
  
data Comment = Comment { cId         :: PostId
                       , cUserName   :: Text
                       , cTime       :: UTCTime
                       , cText       :: Text
                       , cVotes      :: Int
                       , cParent     :: Int
                       , cSubmission :: Int
                       }
             deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Comment)

instance Post Submission where
  pId       = sId
  pTime     = sTime
  pUserName = sUserName
  pVotes    = sVotes

instance Post Comment where
  pId       = cId
  pTime     = cTime
  pUserName = cUserName
  pVotes    = cVotes

  
-------------------------------------------------------------------------------

data Context = Context { database    :: Database
                       , connPool    :: ConnPool Host
                       , sessionUser :: Maybe User
                       , currTime    :: UTCTime
                       }

type ContextM = ServerPartT (ReaderT Context IO)

unpackContext :: Context -> UnWebT (ReaderT Context IO) a -> UnWebT IO a
unpackContext context ct = runReaderT ct context >>= return


class Monad m => MonadContext m where
  getContext :: m Context

instance MonadContext (ServerPartT (ReaderT Context IO)) where
  getContext = ask 

instance MonadContext m => MonadContext (RouteT url m) where
  getContext = lift getContext

instance MonadContext m => MonadContext (XMLGenT (RouteT url m)) where
  getContext = lift getContext

askContext :: MonadContext m => (Context -> r) -> m r
askContext = (`liftM` getContext)

-------------------------------------------------------------------------------
