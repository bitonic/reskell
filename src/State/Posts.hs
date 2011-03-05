{-# LANGUAGE OverloadedStrings #-}
module State.Posts (
  -- From PostMap.hs
  PostId, PostTime, PostScore,
  Post(..), PostIndex(..), PostLookup(..),
  
  PostMap,
  
  Submission(..), Comment(..),
  
  insertPost, emptyPostMap, fromPostList, toPostList, lookupPost
  ) where
  

import Data.ByteString (ByteString)

import Data.Ord (Ordering(..))

import State.Posts.PostMap
import State.Users

data Submission =
  Submission { submissionId :: PostId
             , submissionAuthor :: Username
             , submissionContent :: Either ByteString ByteString
             , submissionVotes :: Int
             , submissionScore :: PostScore
             , submissionTime :: PostTime
             }

instance Eq Submission where
  s1 == s2 = submissionId s1 == submissionId s2
  
instance Post Submission where
  postId = submissionId
  postAuthor = submissionAuthor
  postScore = submissionScore

data Comment =
  Comment { commentId :: PostId
          , commentAuthor :: Username
          , commentContent :: ByteString
          , commentUpVotes :: Int
          , commentDownVotes :: Int
          , commentScore :: PostScore
          , commentTime :: PostTime
          }

instance Eq Comment where
  c1 == c2 = commentId c1 == commentId c2
  
instance Post Comment where
  postId = commentId
  postAuthor = commentAuthor
  postScore = commentScore

type PostMap = IdMap


insertPost :: (Post a) => a -> PostMap a -> PostMap a
insertPost = insertId

emptyPostMap :: (Post a) => PostMap a
emptyPostMap = IdEmpty

fromPostList :: (Post a) => [a] -> PostMap a
fromPostList = fromListId

toPostList :: (Post a) => IdMap a -> [a]
toPostList = toListId

lookupPost :: (Post a) => PostLookup -> PostMap a -> [a]
lookupPost = lookupIdIndex
