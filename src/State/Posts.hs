module State.Posts (
  PostId, PostTime, PostAuthor, PostScore, PostVotes, PostContent(..), Post(..),
  
  getPost, insertSubmission
  ) where

import State.Users
import Config

import Database.Redis.Monad
import Database.Redis.Monad.State
import Database.Redis.ByteStringClass

import Data.Time.Clock

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import Happstack.Server

type PostId = Int
type PostTime = UTCTime
type PostAuthor = Username
type PostScore = Float
type PostVotes = Int
data PostContent = Link ByteString ByteString  -- Title and url
                 | Ask ByteString ByteString   -- Title and text
                 | Comment PostId ByteString   -- Text and parent post
                 deriving (Show, Read)

data Post = Post { postId :: PostId
                 , postTime :: PostTime
                 , postAuthor :: PostAuthor
                 , postUpVotes :: PostVotes
                 , postDownVotes :: PostVotes
                 , postContent :: PostContent
                 , postComments :: [Post]
                 }
          deriving (Show)


(<:>) :: (BS s1, BS s2) => s1 -> s2 -> ByteString
s1 <:> s2 = B.concat [toBS s1, toBS ":", toBS s2]

idCounter = redisPrefix <:> "idCounter"

links = redisPrefix <:> "links"
asks = redisPrefix <:> "asks"
submissions = redisPrefix <:> "submissions"
comments = redisPrefix <:> "comments"
postKey pid = redisPrefix <:> "posts" <:> pid

getPostParam post p =
  liftM (read . fromJust) (hget post p >>= fromRBulk)



insertPost :: PostId -> Username -> PostContent -> RedisM ()
insertPost pid author content = do
  time <- liftIO $ getCurrentTime
  hmset (postKey pid) [ ("id", show pid)
                      , ("time", show time)
                      , ("author", show author)
                      , ("upVotes", show 0)
                      , ("downVotes", show 0)
                      , ("content", show content)
                      ]
  return ()

getPostSingle :: PostId -> RedisM Post
getPostSingle pid = do
  pid <- getPostParam key "id"
  time <-  getPostParam key "time"
  author <-  getPostParam key "author"
  upVotes <-  getPostParam key "upVotes"
  downVotes <-  getPostParam key "downVotes"
  content <-  getPostParam key "content"
  return $ Post { postId = pid
                , postTime = time
                , postAuthor = author
                , postUpVotes = upVotes
                , postDownVotes = downVotes
                , postContent = content
                , postComments = []
                }
  where key = postKey pid

getPost :: PostId -> RedisM Post
getPost pid = do
  post <- getPostSingle pid
  commentsIds <- liftM fromJust $
                 (lrange (postKey pid <:> "comments") takeAll) >>= fromRMulti
  comments <- mapM (\b -> fromRBulk b >>= getPost . read . fromJust) commentsIds
  return post { postComments = comments }
  

insertSubmission :: Username -> ByteString -> ByteString -> RedisM PostId
insertSubmission user title url = do
  pid <- incr idCounter >>= fromRInt
  insertPost pid user (Link title url)
  return pid

postQuery :: RedisM a -> IO a 
postQuery q = redisConn >>= \c -> runWithRedis c q