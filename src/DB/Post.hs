{-# Language OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}

module DB.Post (
    SubmissionType (..)
  , PostId
  , Submission (..)
  , Comment (..)
  , Post (..)
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

import Data.Data               (Data, Typeable)
import Data.Time.Clock
import Data.Bson.Mapping
import Data.Text               (Text)
import Data.Word               (Word32)

import Database.MongoDB

import DB.Common
import DB.User



data SubmissionType = Ask | Link
                    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val SubmissionType where
  val     = val . show
  cast' v = liftM read $ cast' v

type PostId   = Int

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


class (Bson a, Typeable a) => Post a where
  postId :: a -> PostId

instance Post Submission where
  postId = submissionId

instance Post Comment where
  postId = commentId
  

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
getComment    :: DbAccess m => PostId -> m (Maybe Submission)
getComment id'     = getItem $ select [$(getLabel 'commentId)   =: id'] postColl

getPost :: (Post a, DbAccess m) => PostId -> m (Maybe a)
getPost id' = getItem $ select [ "$or" =: [[$(getLabel 'submissionId) =: id']
                                          ,[$(getLabel 'commentId)    =: id']]]
                        postColl

getPosts :: (Post a, DbAccess m) => Query -> m [a]
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