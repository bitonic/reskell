{-# Language OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}

module DB.Post (
    newSubmission
  , newComment
  , getSubmission
  , getComment
  , getPost
  , getLinks
  , getAsks
  , getComments
  ) where

import Prelude hiding (lookup)

import Control.Monad           (liftM)
import Control.Monad.Trans     (liftIO, MonadIO)

import Data.Time.Clock
import Data.Bson.Mapping
import Data.Text               (Text)
import Data.Word               (Word32)

import Database.MongoDB

import DB.Common
import Types


postColl :: Collection
postColl = "post"

postCounter :: String
postCounter = "postCounter"


incPostCounter :: DbAccess m => m PostId
incPostCounter =
  runCommand [ "findAndModify" =: postColl
             , "query"         =: ["_id" =: postCounter]
             , "new"           =: True
             , "update"        =: ["$inc" =: ["counter" =: (1 :: PostId)]]
             , "upsert"        =: True
             ] >>= lookup "value" >>= lookup "counter" 
  

newSubmission :: (MonadIO m, DbAccess m)
                 => UserName -> Text -> SContent -> m Submission
newSubmission username title content = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let submission = Submission { sId       = id'
                              , sUserName = username
                              , sTime     = time
                              , sTitle    = title
                              , sContent  = content
                              , sVotes    = 0
                              }
  insert_ postColl $ toBson submission
  return submission

newComment :: (MonadIO m, DbAccess m, Post a)
              => UserName -> Text -> Submission -> a -> m Comment
newComment username text submission parent = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let comment = Comment { cId         = id'
                        , cTime       = time
                        , cUserName   = username
                        , cText       = text
                        , cVotes      = 0
                        , cParent     = pId parent
                        , cSubmission = sId submission
                        }
  insert_ postColl $ toBson comment
  return comment


getSubmission :: DbAccess m => PostId -> m (Maybe Submission)
getSubmission id' = getItem $ select [$(getLabel 'sId) =: id'] postColl
getComment    :: DbAccess m => PostId -> m (Maybe Comment)
getComment    id' = getItem $ select [$(getLabel 'cId) =: id'] postColl


getPost :: DbAccess m => PostId -> m (Maybe (Either Submission Comment))
getPost id' = do
  pM <- findOne $ select ["$or" =: [[$(getLabel 'sId) =: id']
                                  ,[$(getLabel 'cId) =: id']]] postColl
  return $ pM >>= \p -> case fromBson p :: Maybe Submission of
    Just s  -> Just $ Left s
    Nothing -> liftM Right (fromBson p :: Maybe Comment)

getPosts :: (Bson a, DbAccess m) => Query -> m [a]
getPosts q = find q >>= rest >>= mapM fromBson

getLinks, getAsks :: DbAccess m => Limit -> Word32 -> m [Submission]
getLinks l s =
  getPosts (select (subDocument $(getLabel 'sContent) $(getConsDoc 'Link))
            postColl) { limit = l
                      , skip = s 
                      , sort = [ $(getLabel 'sContent) =: (-1 :: Int) ]
                      }  
getAsks l s =
  getPosts (select (subDocument $(getLabel 'sContent) $(getConsDoc 'Ask))
            postColl) { limit = l
                      , skip = s
                      , sort = [ $(getLabel 'sContent) =: (-1 :: Int) ]
                      }

getComments :: (DbAccess m, Post a) => a -> m [Comment]
getComments a = getPosts (select [ $(getLabel 'cParent) =: pId a ] postColl)