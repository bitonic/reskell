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

import Prelude hiding (concat)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString.UTF8 as U
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


joinBS [bs] = bs
joinBS (bs : rest) = concat [bs, U.fromString "\x9999", joinBS rest]

splitBS bs | U.length rest == 0 = [bs']
           | otherwise = bs' : splitBS (snd $ U.break (/= '\x9999') rest)
  where
    (bs', rest) = U.break (== '\x9999') bs

instance BS PostContent where
  toBS (Link bs1 bs2) = joinBS [U.fromString "link", bs1, bs2]
  toBS (Ask bs1 bs2) = joinBS [U.fromString "ask", bs1, bs2]
  toBS (Comment id bs) = joinBS [U.fromString "comment", toBS id, bs]

  fromBS bs | t == U.fromString "link" = Link (fromBS fst) (fromBS snd)
            | t == U.fromString "ask" = Ask (fromBS fst) (fromBS snd)
            | t == U.fromString "comment" = Comment (fromBS fst) (fromBS snd)
    where (t : fst : snd : _) = splitBS bs

instance BS UTCTime where
  toBS = U.fromString . show
  fromBS = read . U.toString


data Post = Post { postId :: PostId
                 , postTime :: PostTime
                 , postAuthor :: PostAuthor
                 , postVotes :: PostVotes
                 , postContent :: PostContent
                 , postComments :: [Post]
                 }
          deriving (Show)


(<:>) :: (BS s1, BS s2) => s1 -> s2 -> ByteString
s1 <:> s2 = concat [toBS s1, toBS ":", toBS s2]

idCounter = redisPrefix <:> "idCounter"

links = redisPrefix <:> "links"
asks = redisPrefix <:> "asks"
submissions = redisPrefix <:> "submissions"
comments = redisPrefix <:> "comments"
postKey pid = redisPrefix <:> "posts" <:> pid

getPostParam post p =
  liftM (fromBS . fromJust) (hget post p >>= fromRBulk)



insertPost :: PostId -> Username -> PostContent -> RedisM ()
insertPost pid author content = do
  time <- liftIO $ getCurrentTime
  hmset (postKey pid) [ ("id", toBS pid)
                      , ("time", toBS time)
                      , ("author", author)
                      , ("votes", toBS (0 :: Int))
                      , ("content", toBS content)
                      ]
  return ()

getPostSingle :: PostId -> RedisM Post
getPostSingle pid = do
  pid <- getPostParam key "id"
  time <-  getPostParam key "time"
  author <-  getPostParam key "author"
  votes <-  getPostParam key "votes"
  content <-  getPostParam key "content"
  return $ Post { postId = pid
                , postTime = time
                , postAuthor = author
                , postVotes = votes
                , postContent = content
                , postComments = []
                }
  where key = postKey pid

getPost :: PostId -> RedisM Post
getPost pid = do
  post <- getPostSingle pid
  commentsIds <- liftM fromJust $
                 (lrange (postKey pid <:> "comments") takeAll) >>= fromRMulti
  comments <- mapM (\b -> fromRBulk b >>= getPost . fromBS . fromJust) commentsIds
  return post { postComments = comments }
  
postQuery :: RedisM a -> IO a
postQuery q = redisConn >>= \c -> runWithRedis c q