{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators, 
    FlexibleInstances, StandaloneDeriving #-}

module State.Posts (
  PostId, PostTime, PostScore, PostVotes, PostContent(..), Post(..),
  
  insertPost, getPostSingle, getPost
  ) where

import Config
import State.Users

import Database.Redis.Monad as R
import Database.Redis.Monad.State
import Database.Redis.ByteStringClass

import Data.Data (Data, Typeable)

import Data.Time.Clock

import Prelude hiding (concat)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString.UTF8 as U
import Data.Maybe (fromJust, fromMaybe)

import Data.HashMap (HashMap)
import qualified Data.HashMap as M

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify)
import qualified Control.Monad.Reader as Reader -- Since we have conflicts with asks

import Data.List (delete)

import Happstack.Server (ServerPart)
import Happstack.State
import Happstack.Data.Serialize

-------------------------------------------------------------------------

type PostId = Int
type PostTime = UTCTime
type PostScore = Float
type PostVotes = Int
data PostContent = Link ByteString ByteString  -- Title and url
                 | Ask ByteString ByteString   -- Title and text
                 | Comment PostId PostId ByteString   -- Text and parent post
                 deriving (Show, Read)

data Post = Post { postId :: PostId
                 , postTime :: PostTime
                 , postUser :: Username
                 , postVotes :: PostVotes
                 , postContent :: PostContent
                 , postComments :: [Post]
                 }
          deriving (Show, Read)

-------------------------------------------------------------------------
-- redis functions

joinBS [bs] = bs
joinBS (bs : rest) = concat [bs, U.fromString "\x9999", joinBS rest]

splitBS bs | U.length rest == 0 = [bs']
           | otherwise = bs' : splitBS (snd $ U.break (/= '\x9999') rest)
  where
    (bs', rest) = U.break (== '\x9999') bs

instance BS PostContent where
  toBS (Link bs1 bs2) = joinBS [U.fromString "link", bs1, bs2]
  toBS (Ask bs1 bs2) = joinBS [U.fromString "ask", bs1, bs2]
  toBS (Comment id1 id2 bs) = joinBS [U.fromString "comment", toBS id1, toBS id2, bs]

  fromBS bs | t == U.fromString "link" = Link (fromBS $ contents !! 0)
                                             (fromBS $ contents !! 1)
            | t == U.fromString "ask" = Ask (fromBS $ contents !! 0)
                                           (fromBS $ contents !! 1)
            | t == U.fromString "comment" = Comment (fromBS $ contents !! 0)
                                                   (fromBS $ contents !! 1)
                                                   (fromBS $ contents !! 2)
    where (t : contents) = splitBS bs

instance BS UTCTime where
  toBS = U.fromString . show
  fromBS = read . U.toString

(<:>) :: (BS s1, BS s2) => s1 -> s2 -> ByteString
s1 <:> s2 = concat [toBS s1, toBS ":", toBS s2]

idCounter = redisPrefix <:> "idCounter"

postKey pid = redisPrefix <:> "posts" <:> pid

postCommentsKey pid = postKey pid <:> "comments"

getPostParam post p =
  liftM (fromBS . fromJust) (hget post p >>= fromRBulk)


insertPostR :: Username -> PostContent -> RedisM PostId
insertPostR user content = do
  pid <- incr idCounter >>= fromRInt
  time <- liftIO $ getCurrentTime
  hmset (postKey pid) [ ("id", toBS pid)
                      , ("time", toBS time)
                      , ("user", user)
                      , ("votes", toBS (0 :: Int))
                      , ("content", toBS content)
                      ]
  return pid

getPostSingleR :: PostId -> RedisM Post
getPostSingleR pid = do
  pid <- getPostParam key "id"
  time <-  getPostParam key "time"
  user <-  getPostParam key "user"
  votes <-  getPostParam key "votes"
  content <-  getPostParam key "content"
  return $ Post { postId = pid
                , postTime = time
                , postUser = user
                , postVotes = votes
                , postContent = content
                , postComments = []
                }
  where key = postKey pid

getPostR :: PostId -> RedisM Post
getPostR pid = do
  post <- getPostSingleR pid
  commentsIds <- liftM fromJust $
                 (lrange (postCommentsKey pid) takeAll) >>= fromRMulti
  comments <- mapM (\b -> fromRBulk b >>= getPostR . fromBS . fromJust) commentsIds
  return post { postComments = comments }

deletePostR :: PostId -> RedisM ()
deletePostR pid = do
  getPostParam (postKey pid) "content" >>= deleteComment
  deletePostR' pid
  where
    deleteComment (Comment parentid _ _) =
      lrem (postCommentsKey parentid) 0 pid >> return ()
    deleteComment _ = return ()

    deletePostR' :: PostId -> RedisM ()
    deletePostR' pid = do
      del (postKey pid)
      commentsIds <- liftM (map (fromBS . fromJust) . fromJust) $
                     lrange (postCommentsKey pid) takeAll >>= fromRMultiBulk
      del (postCommentsKey pid)  
      mapM_ deletePostR' commentsIds
      
postQuery :: RedisM a -> IO a
postQuery q = redisConn >>= \c -> runWithRedis c q

------------------------------------------------------------------------------
-- state functions

type UserPosts = HashMap Username [PostId]

data Posts = Posts { submissions :: [PostId]
                   , asks :: [PostId]
                   , links :: [PostId]
                   , comments :: [PostId]
                   , usersSubmissions :: UserPosts
                   , usersComments :: UserPosts
                   }
           deriving (Read, Show, Eq, Ord, Data, Typeable)
instance Version Posts
$(deriveSerialize ''Posts)

instance Component Posts where
  type Dependencies Posts = Users :+: End
  initialValue = Posts [] [] [] [] M.empty M.empty


addUserPost :: PostId -> Username -> UserPosts -> UserPosts
addUserPost pid user map =
  M.insert user (pid : (fromMaybe [] $ M.lookup user map)) map
  
deleteUserPost :: PostId -> Username -> UserPosts -> UserPosts
deleteUserPost pid user map =
  M.insert user (delete pid (map M.! user)) map

insertSubmissionS :: PostId -> Username -> Update Posts ()
insertSubmissionS pid user =
  modify (\s -> s { submissions = pid : submissions s
                 , usersSubmissions = addUserPost pid user (usersSubmissions s)
                 })
  
insertAskS :: PostId -> Username -> Update Posts ()
insertAskS pid user = do
  insertSubmissionS pid user
  modify (\s -> s { asks = pid : asks s })

insertLinkS :: PostId -> Username -> Update Posts ()
insertLinkS pid user = do
  insertSubmissionS pid user
  modify (\s -> s { links = pid : links s })

insertCommentS :: PostId -> Username -> Update Posts ()
insertCommentS pid user =
  modify (\s -> s { comments = pid : comments s
                 , usersComments = addUserPost pid user (usersComments s)
                 })

getSubmissionsS, getAsksS, getCommentsS :: Query Posts [PostId]
getSubmissionsS = Reader.asks submissions
getAsksS = Reader.asks asks
getCommentsS = Reader.asks comments


deleteSubmissionS :: PostId -> Username -> Update Posts ()
deleteSubmissionS pid user =
  modify (\s -> s { submissions = delete pid (submissions s)
                 , usersSubmissions = deleteUserPost pid user (usersSubmissions s)
                 })

deleteLinkS, deleteAskS, deleteCommentS :: PostId -> Username -> Update Posts ()
deleteLinkS pid user = deleteSubmissionS pid user >>
                  modify (\s -> s { links = delete pid (links s) })
deleteAskS pid user = deleteSubmissionS pid user >>
                 modify (\s -> s { asks = delete pid (asks s) })
deleteCommentS pid user =
  modify (\s -> s { comments = delete pid (comments s)
                 , usersComments = deleteUserPost pid user (usersComments s)
                 })

$(mkMethods ''Posts [ 'insertAskS, 'insertLinkS, 'insertCommentS
                    , 'getSubmissionsS, 'getAsksS, 'getCommentsS
                    , 'deleteLinkS, 'deleteAskS, 'deleteCommentS
                    ])

------------------------------------------------------------------------------
-- exported functions

insertPost :: Username -> PostContent -> ServerPart PostId
insertPost user content = do
  case content of
    (Link _ _)      -> insert InsertLinkS
    (Ask _ _)       -> insert InsertAskS
    (Comment _ _ _) -> insert InsertCommentS
  where
    insert ev = do
      pid <- liftIO . postQuery $ insertPostR user content
      update $ ev pid user
      return pid

getPostSingle :: PostId -> ServerPart Post
getPostSingle pid = liftIO . postQuery $ getPostSingleR pid

getPost :: PostId -> ServerPart Post
getPost pid = liftIO . postQuery $ getPostR pid

deletePost :: PostId -> ServerPart ()
deletePost pid = do
  deletePostState
  liftIO . postQuery $ deletePostR pid
  where
    deletePostState = do
      post <- getPost pid
      let user = postUser post
      case postContent post of
        (Link _ _)      -> update $ DeleteLinkS pid user
        (Ask _ _)       -> update $ DeleteAskS pid user
        (Comment _ _ _) -> update $ DeleteCommentS pid user
      mapM_ deletePost $ map postId $ postComments post