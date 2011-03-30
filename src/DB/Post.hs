{-# Language OverloadedStrings, DeriveDataTypeable #-}

module DB.Post where

import Prelude hiding (lookup)

import Control.Monad           (liftM)
import Control.Monad.IO.Class

import Data.Data               (Data, Typeable)
import Data.Maybe              (fromJust)
import Data.Time.Clock

import Database.MongoDB

import DB.Common



data SubmissionType = Ask | Link
                    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

instance Val SubmissionType where
  val     = val . show
  cast' v = liftM read $ cast' v

data Submission = Submission { submissionId       :: ObjectId
                             , submissionUserName :: UString
                             , submissionTime     :: UTCTime
                             , submissionTitle    :: UString
                             , submissionType     :: SubmissionType
                             , submissionContent  :: UString
                             , submissionVotes    :: Int
                             , submissionComments :: [Comment]
                             }
                deriving (Eq, Ord, Show)
instance Bson Submission where
  toBson s = return [ "_id"      =: submissionId s
                    , "username" =: submissionUserName s
                    , "time"     =: submissionTime s
                    , "title"    =: submissionTitle s
                    , "type"     =: submissionType s
                    , "content"  =: submissionContent s
                    , "votes"    =: submissionVotes s
                    , "comments" =: map commentId (submissionComments s)
                    ]
  fromBson doc = do
    id' <- lookup "_id" doc
    userName <- lookup "username" doc
    time <- lookup "time" doc
    title <- lookup "title" doc
    type' <- lookup "type" doc
    content <- lookup "content" doc
    votes <- lookup "votes" doc
    comments <- lookup "comments" doc >>= mapM fromBson
    return Submission { submissionId       = id'
                      , submissionUserName = userName
                      , submissionTime     = time 
                      , submissionTitle    = title
                      , submissionType     = type'
                      , submissionContent  = content
                      , submissionVotes    = votes
                      , submissionComments = comments
                      }
      


data Comment = Comment { commentId       :: ObjectId
                       , commentUserName :: UString
                       , commentTime     :: UTCTime
                       , commentText     :: UString
                       , commentVotes    :: Int
                       , commentComments :: [Comment]
                       }
             deriving (Eq, Ord, Show)
instance Bson Comment where
  toBson c = return [ "_id"      =: commentId c
                    , "username" =: commentUserName c
                    , "time"     =: commentTime c
                    , "text"     =: commentText c
                    , "votes"    =: commentVotes c
                    , "comments" =: map commentId (commentComments c)
                    ]
  fromBson doc = do
    id' <- lookup "_id" doc
    userName <- lookup "username" doc
    time <- lookup "time" doc
    text <- lookup "text" doc
    votes <- lookup "votes" doc
    comments <- lookup "comments" doc >>= mapM fromBson
    return Comment { commentId       = id'
                   , commentUserName = userName
                   , commentTime     = time 
                   , commentText     = text
                   , commentVotes    = votes
                   , commentComments = comments
                   }

class Post a where
  postId :: a -> ObjectId

instance Post Submission where
  postId = submissionId

instance Post Comment where
  postId = commentId

postCollection :: Collection
postCollection = "post"

newSubmission :: (MonadIO m, DbAccess m)
                 => UString -> UString -> SubmissionType -> UString -> m ObjectId
newSubmission username title type' content = do
  time <- liftIO getCurrentTime
  id' <- insert postCollection [ "username" =: username
                              , "time"     =: time
                              , "title"    =: title
                              , "type"     =: type'
                              , "content"  =: content
                              , "votes"    =: (0 :: Int)
                              , "comments" =: ([] :: [ObjectId])
                              ]
  return $ fromJust $ cast' id'

updatePost :: (Bson a, Post a, DbAccess m) => a -> m ()
updatePost p = updateItem (Select [ "_id" =: postId p ] postCollection) p

getPost :: (Bson a, Post a, Val v, DbAccess m) => v -> m (Maybe a)
getPost id' = getItem (select [ "_id" =: id' ] postCollection)