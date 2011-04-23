{-# Language TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Post (
    PostId
  , SContent (..)
  , getDomain
  , Submission (..)
  , Comment (..)
  , Post (..)
  ) where

import Control.Monad.Reader
import Control.Monad.Identity  (Identity)

import Data.Bson.Mapping
import Data.Data               (Data, Typeable)
import Data.Time.Clock         (UTCTime)

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator

import Web.Routes              ()

import Types.User


type PostId = Int

class (Bson a, Typeable a) => Post a where
  pId       :: a -> PostId
  pTime     :: a -> UTCTime
  pUserName :: a -> UserName
  pVoters   :: a -> [UserName]
  

data SContent = Ask String  
              | Link String String -- ^ url, domain
              deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveBson ''SContent)


parseProtocol :: ParsecT String u Identity String
parseProtocol = string "http://" <|> string "https://"

parseDomain :: ParsecT String u Identity String
parseDomain = do
  parseProtocol
  d1 <- liftM ('.' :) domainLetters
  d2 <- domainSeg
  rest <- manyTill domainSeg (eof <|> (char '/' >> return ()))
  let all' = d1 : d2 : rest
  return $ tail $ concat $ drop (length all' - 2) all'
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


data Submission = Submission { sId        :: PostId
                             , sUserName  :: UserName
                             , sTime      :: UTCTime
                             , sTitle     :: String
                             , sContent   :: SContent
                             , sVotesUp   :: Int
                             , sVotesDown :: Int
                             , sScore     :: Double
                             , sVoters    :: [UserName]
                             }
                deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Submission)

  
  
data Comment = Comment { cId         :: PostId
                       , cUserName   :: String
                       , cTime       :: UTCTime
                       , cText       :: String
                       , cVotesUp    :: Int
                       , cVotesDown  :: Int
                       , cParent     :: Int
                       , cSubmission :: Int
                       , cScore      :: Double
                       , cVoters     :: [UserName]
                       }
             deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveBson ''Comment)

instance Post Submission where
  pId       = sId
  pTime     = sTime
  pUserName = sUserName
  pVoters   = sVoters

instance Post Comment where
  pId       = cId
  pTime     = cTime
  pUserName = cUserName
  pVoters   = cVoters
