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
  , SubmissionType (..)
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
import Data.Text.Encoding      (encodeUtf8, decodeUtf8)
import Data.ByteString         (ByteString)
import Data.Time.Clock         (UTCTime)
import Data.CompactString.UTF8 (fromByteString_, toByteString)


import Happstack.Server        (Conf, ServerPartT, UnWebT)

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

data User = User { userName :: UserName
                 , userPassword :: Password
                 , userRank :: UserRank
                 , userAbout :: Text
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveBson ''User)

hashStrength :: Int
hashStrength = 12

hashUserPassword :: User -> IO User
hashUserPassword user = do
  hashedp <- makePassword (userPassword user) hashStrength
  return user { userPassword = hashedp }


-------------------------------------------------------------------------------

type PostId = Int

class (Bson a, Typeable a) => Post a where
  postId       :: a -> PostId
  postTime     :: a -> UTCTime
  postUserName :: a -> UserName
  postVotes    :: a -> Int
  

data SubmissionType = Ask | Link
                    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val SubmissionType where
  val     = val . show
  cast' v = cast' v >>= readM
  

data Submission = Submission { submissionId       :: PostId
                             , submissionUserName :: UserName
                             , submissionTime     :: UTCTime
                             , submissionTitle    :: Text
                             , submissionType     :: SubmissionType
                             , submissionContent  :: Text
                             , submissionVotes    :: Int
                             }
                deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Submission)

  
  
data Comment = Comment { commentId         :: PostId
                       , commentUserName   :: Text
                       , commentTime       :: UTCTime
                       , commentText       :: Text
                       , commentVotes      :: Int
                       , commentParent     :: Int
                       , commentSubmission :: Int
                       }
             deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Comment)

instance Post Submission where
  postId       = submissionId
  postTime     = submissionTime
  postUserName = submissionUserName
  postVotes    = submissionVotes

instance Post Comment where
  postId       = commentId
  postTime     = commentTime
  postUserName = commentUserName
  postVotes    = commentVotes


{-
data SubmissionC = Ask Text Text
                 | Link Text
                 deriving (Eq, Ord, Read, Show, Data, Typeable)

submissionTypeLabel :: String
submissionTypeLabel = "type"

instance Bson SubmissionC where
  toBson (Ask title content) = [ u "type"    =: $(getLabel 'Ask)
                               , u "title"   =: title
                               , u "content" =: content
                               ]
  toBson (Link url)          = [ u "type"    =: $(getLabel 'Link)
                               , u "url"     =: url
                               ]
  
  fromBson doc = do
    type' <- lookup (u "type") doc
    case type' of
      "Ask" -> do
        title <- lookup (u "title") doc
        content <- lookup (u "content") doc
        return $ Ask title content
      _ -> do
        url <- lookup (u "url") doc
        return $ Link url

instance Val SubmissionC where
  val = Doc . toBson
  cast' (Doc doc) = fromBson doc
  cast' _ = Nothing
-}
  
-------------------------------------------------------------------------------

data Context = Context { httpConf    :: Conf
                       , static      :: FilePath
                       , database    :: Database
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
