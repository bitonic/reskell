{-# Language OverloadedStrings, DeriveDataTypeable, TemplateHaskell,
    ExistentialQuantification #-}

module DB.Post (
    PostE (..)
  , newSubmission
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
import Control.Monad.IO.Class

import Data.Time.Clock
import Data.Bson.Mapping
import Data.Text               (Text)
import Data.Word               (Word32)

import Database.MongoDB

import DB.Common
import Types


data PostE = forall p. Post p => PostE p

postColl :: Collection
postColl = "post"

postCounter :: String
postCounter = "postCounter"

incPostCounter :: DbAccess m => m PostId
incPostCounter =
  runCommand [ "findAndModify" =: postColl
             , "query"         =: [ "_id" =: postCounter ]
             , "new"           =: True
             , "update"        =: [ "$inc" =: [ "counter" =: (1 :: PostId) ] ]
             , "upsert"        =: True
             ] >>= lookup "value" >>= lookup "counter" 
  

newSubmission :: (MonadIO m, DbAccess m)
                 => UserName -> Text -> SubmissionType -> Text -> m PostId
newSubmission username title type' content = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let submission = Submission { submissionId       = id'
                              , submissionUserName = username
                              , submissionTime     = time
                              , submissionTitle    = title
                              , submissionType     = type'
                              , submissionContent  = content
                              , submissionVotes    = 0
                              }
  insert_ postColl $ toBson submission
  return id'

newComment :: (MonadIO m, DbAccess m, Post a)
              => UserName -> Text -> Submission -> a -> m PostId
newComment username text submission parent = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let comment = Comment { commentId         = id'
                        , commentTime       = time
                        , commentUserName   = username
                        , commentText       = text
                        , commentVotes      = 0
                        , commentParent     = postId parent
                        , commentSubmission = submissionId submission
                        }
  insert_ postColl $ toBson comment
  return id'


getSubmission :: DbAccess m => PostId -> m (Maybe Submission)
getSubmission id' = getItem $ select [$(getLabel 'submissionId) =: id'] postColl
getComment    :: DbAccess m => PostId -> m (Maybe Comment)
getComment    id' = getItem $ select [$(getLabel 'commentId)    =: id'] postColl

getPost :: DbAccess m => PostId -> m (Maybe PostE)
getPost id' = do
  p <- findOne $ select ["$or" =: [[$(getLabel 'submissionId) =: id']
                                 ,[$(getLabel 'commentId)    =: id']]] postColl
  return $ case p of
    Nothing  -> Nothing
    Just doc -> case (fromBson doc :: Maybe Submission) of
      Just s  -> Just $ PostE s
      Nothing -> liftM PostE (fromBson doc :: Maybe Comment)


getPosts :: (Bson a, DbAccess m) => Query -> m [a]
getPosts q = find q >>= rest >>= mapM fromBson

getLinks, getAsks :: DbAccess m => Limit -> Word32 -> m [Submission]
getLinks l s =
 getPosts (select [$(getLabel 'submissionType) =: Link] postColl)
                      { limit = l
                      , skip = s 
                      , sort = [ $(getLabel 'submissionTime) =: (-1 :: Int) ]
                      }  
getAsks l s =
  getPosts (select [$(getLabel 'submissionType) =: Ask] postColl)
                      { limit = l
                      , skip = s
                      , sort = [ $(getLabel 'submissionTime) =: (-1 :: Int) ]
                      }

getComments :: (DbAccess m, Post a) => a -> m [Comment]
getComments a = getPosts (select [ $(getLabel 'commentParent) =: postId a ] postColl)