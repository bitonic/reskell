{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module State.Posts where

import Control.Monad (liftM)
import Control.Monad.Reader (asks)

import Data.Either
import Data.Data (Data, Typeable)
import Data.ByteString (ByteString)

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Data.Hashable (Hashable)

import Happstack.State

import State.Users

type PostId = Int
type PostTime = Int
type PostScore = Float
type PostUrl = ByteString
type PostSeq = Seq PostId


--------------------------------------------------------------------------------

instance Version (Seq a) where mode = Primitive
instance (Serialize a, Ord a, Hashable a) => Serialize (Seq a) where
  getCopy = contain $ fmap Seq.fromList safeGet
  putCopy = contain . safePut . Fold.toList
  
--------------------------------------------------------------------------------

data Post = Post { postId :: PostId
                 , postContent :: Either ByteString PostUrl
                 , postUpVotes :: Int
                 , postDownVotes :: Int
                 , postTime :: PostTime
                 , postComments :: PostSeq
                 }
          deriving (Eq, Ord, Read, Show, Data, Typeable)

{-
instance Eq Post where
  p1 == p2 = postId p1 == postId p2

instance Ord Post where
  comparable p1 p2 = comparable (postId p1) (postId p2)
-}

instance Version Post
$(deriveSerialize ''Post)

postScore :: Post -> PostTime -> PostScore
postScore p now = tf (postUpVotes p - postDownVotes p - 1) /
                  ((tf (now - postTime p) / 60) ** 1.5)
  where tf = fromInteger . toInteger

type PostMap = IntMap Post

data Posts = Posts { postMap :: PostMap
                   , newSubmissions :: PostSeq
                   , hotSubmissions :: PostSeq
                   , allComments :: PostSeq
                   }
           deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version Posts
$(deriveSerialize ''Posts)

-- Queries

slicePosts :: (Monad m) => Int -> Int -> m PostSeq -> m PostSeq
slicePosts from n = liftM (Seq.take n . Seq.drop from)

getHotSubmissions :: Int -> Int -> Query Posts PostSeq
getHotSubmissions from n = slicePosts from n $ asks hotSubmissions

getNewSubmissions :: Int -> Int -> Query Posts PostSeq
getNewSubmissions from n = slicePosts from n $ asks newSubmissions

getAllComments :: Int -> Int -> Query Posts PostSeq
getAllComments from n = slicePosts from n $ asks allComments

getComments :: Int -> Int -> Post -> PostSeq
getComments from n post = Seq.take n . Seq.drop from . postComments $ post

getPost :: PostId -> Query Posts (Maybe Post)
getPost id = liftM (M.lookup id) $ asks postMap

-- Updates... the hard part