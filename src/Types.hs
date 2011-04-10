{-# Language DeriveDataTypeable, TemplateHaskell, FlexibleInstances, 
    FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
  -- * The application monad
    AppM
  , unpackApp
  , AppError (..)
  , notFoundError
  , serverError
  , databaseError
    
  -- * Context
  , Context (..)
  , MonadContext (..)
  , askContext
    
  -- * User  
  , UserRank (..)
  , User (..)
  , UserName
  , Password
  , Session
  , genSessionId
    
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



import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent.MVar

import Data.Bson               
import Data.Bson.Mapping
import Data.Data               (Data, Typeable)
import Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Time.Clock         (UTCTime)

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator

import Happstack.Server hiding (Host)

import Database.MongoDB        (Database (..), ConnPool, Host, Failure)

import Web.Routes              (RouteT)

import HSP                     (XMLGenT)

import Crypto.PasswordStore

import Numeric                 (showHex)


readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse."
  where
    res = reads s


-------------------------------------------------------------------------------
  
instance Val ByteString where
  val     = val . Binary
  cast' b = case cast' b of
    Just (Binary bs) -> Just bs
    Nothing          -> Nothing


-------------------------------------------------------------------------------
  
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

-------------------------------------------------------------------------------

type PostId = Int

class (Bson a, Typeable a) => Post a where
  pId       :: a -> PostId
  pTime     :: a -> UTCTime
  pUserName :: a -> UserName
  pVotes    :: a -> Int
  

data SContent = Ask String  
              | Link String String -- ^ url, domain
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

getDomain :: String -> Maybe String
getDomain url = case parse parseDomain "" url of
  Right p -> Just p
  Left _  -> Nothing


data Submission = Submission { sId       :: PostId
                             , sUserName :: UserName
                             , sTime     :: UTCTime
                             , sTitle    :: String
                             , sContent  :: SContent
                             , sVotes    :: Int
                             }
                deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Submission)

  
  
data Comment = Comment { cId         :: PostId
                       , cUserName   :: String
                       , cTime       :: UTCTime
                       , cText       :: String
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
                         
                       , userMVar    :: MVar ()
                       }


data AppError = DatabaseError Failure
              | NotFound
              | ServerError String
              | OtherError String
              deriving (Show)

instance Error AppError where
  noMsg  = OtherError ""
  strMsg = OtherError

type AppM = ServerPartT (ErrorT AppError (ReaderT Context IO))

notFoundError :: MonadError AppError m => m a
notFoundError = throwError NotFound
serverError   :: MonadError AppError m => String -> m a
serverError   = throwError . ServerError
databaseError :: MonadError AppError m => Failure -> m a
databaseError = throwError . DatabaseError

unpackContext :: Context -> UnWebT (ReaderT Context IO) a -> UnWebT IO a
unpackContext context ct = runReaderT ct context >>= return

unpackError :: UnWebT (ErrorT AppError (ReaderT Context IO)) a
               -> UnWebT (ReaderT Context IO) a
unpackError et = do
  eitherV <- runErrorT et
  return $ case eitherV of
    Left err -> Just (Left $ toResponse $ 
                      "Catastrophic failure " ++ show err,
                      filterFun $ \r -> r{rsCode = 500})
    Right x -> x

unpackApp :: Context
             -> UnWebT (ErrorT AppError (ReaderT Context IO)) a
             -> UnWebT IO a
unpackApp context = unpackContext context . unpackError

class Monad m => MonadContext m where
  getContext :: m Context

instance MonadContext (ServerPartT (ErrorT AppError (ReaderT Context IO))) where
  getContext = ask
  
instance MonadContext m => MonadContext (RouteT url m) where
  getContext = lift getContext

instance MonadContext m => MonadContext (XMLGenT (RouteT url m)) where
  getContext = lift getContext

askContext :: MonadContext m => (Context -> r) -> m r
askContext = (`liftM` getContext)
  

-------------------------------------------------------------------------------