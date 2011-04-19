{-# Language OverloadedStrings, DeriveDataTypeable, TemplateHaskell, 
    ScopedTypeVariables #-}

module DB.Post (
    initScoring
  , newSubmission
  , newComment
  , getSubmission
  , getComment
  , getPost
  , getLinks
  , getAsks
  , getComments
  , countComments
  , voteSubmission
  , voteComment
  , votePost
  ) where

import Prelude hiding (lookup)

import Control.Monad           (liftM)
import Control.Monad.Trans     (liftIO, MonadIO)

import Data.Time.Clock
import Data.Bson.Mapping
import Data.Word               (Word32)

import Database.MongoDB

import DB.Common
import Types




postColl :: Collection
postColl = "post"

postCounter :: String
postCounter = "postCounter"


-- | This is to be called when the application starts. It gets the
-- "start" time, or sets it otherwise, then it returns it.
initScoring :: (DbAccess m, MonadIO m) => m UTCTime
initScoring = do
  let id' = "scoringStart" :: String
  start <- findOne (select ["_id" =: id'] postColl)
  case start of
    Nothing -> do
      time <- liftIO getCurrentTime
      insert_ postColl ["_id" =: id', "time" =: time]
      return time
    Just s -> lookup "time" s
  

incPostCounter :: DbAccess m => m PostId
incPostCounter =
  runCommand [ "findAndModify" =: postColl
             , "query"         =: ["_id" =: postCounter]
             , "new"           =: True
             , "update"        =: ["$inc" =: ["counter" =: (1 :: PostId)]]
             , "upsert"        =: True
             ] >>= lookup "value" >>= lookup "counter"
  

newSubmission :: (MonadIO m, DbAccess m, MonadContext m)
                 => UserName -> String -> SContent -> m Submission
newSubmission username title content = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let submission = Submission { sId       = id'
                              , sUserName = username
                              , sTime     = time
                              , sTitle    = title
                              , sContent  = content
                              , sVotes    = 0
                              , sScore    = 0
                              }
  score <- scoreSubmission submission
  insert_ postColl $ toBson $ submission {sScore = score}
  return submission

newComment :: (MonadIO m, DbAccess m, Post a)
              => UserName -> String -> PostId -> a -> m Comment
newComment username text submissionid parent = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let comment = Comment { cId         = id'
                        , cTime       = time
                        , cUserName   = username
                        , cText       = text
                        , cUpVotes    = 0
                        , cDownVotes  = 0
                        , cParent     = pId parent
                        , cSubmission = submissionid
                        , cScore      = 0
                        }
  insert_ postColl $ toBson $ comment {cScore = scoreComment comment}
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
getComments p = getPosts (select [$(getLabel 'cParent) =: pId p] postColl)

countComments :: (DbAccess m, Post a) => a -> m Int
countComments p = count (select [$(getLabel 'cParent) =: pId p] postColl)


-- | Scores a post. Taken straight from the reddit algorithm.
scoreSubmission :: (MonadContext m, DbAccess m) => Submission -> m Double
scoreSubmission submission = do
  start <- askContext scoringStart
  let fi      = fromInteger . toInteger
      s       = sVotes submission
      order   = logBase 10 $ max (fi $ abs s) 1
      sign    | s > 0     = 1
              | s < 0     = -1
              | otherwise = 0
      seconds = realToFrac $ diffUTCTime (sTime submission) start
  return $ order + sign * seconds / 45000


-- | Scores the comment,
-- <http://www.evanmiller.org/how-not-to-sort-by-average-rating.html>
scoreComment :: Comment -> Double
scoreComment comment =
  if n == 0
  then 0
  else phat+z*z/(2*n)-z*(sqrt ((phat*(1-phat)+z*z/(4*n))/n))/(1+z*z/n)
  where
    pos  = realToFrac $ cUpVotes comment
    n    = pos + (realToFrac $ cDownVotes comment)
    phat = pos / n
    z    = 1.644853646608357 -- 95% confidence, Statistics2.pnormaldist(1-0.2/2)
    
  
                                   
voteSubmission :: (MonadContext m, DbAccess m) => Submission -> m ()
voteSubmission s = do
  s' <- runCommand
        [ "findAndModify" =: postColl
        , "query"         =: [$(getField 'sId) s]
        , "new"           =: True
        , "update"        =: ["$inc" =: [$(getLabel 'sVotes) =: (1 :: Int)]]
        ] >>= lookup "value" >>= fromBson
  score <- scoreSubmission s'
  -- Note that I select the post based on the id *and* the number of
  -- votes. In this way we are sure of updating the post only if the
  -- post is in the same state that we received it. In this way we
  -- avoid race conditions.
  runCommand [ "findAndModify" =: postColl
             , "query"         =: [$(getField 'sId) s', $(getField 'sVotes) s']
             , "update"        =: ["$set" =: [$(getLabel 'sScore) := (Float score)]]
             ]
  return ()


voteComment :: DbAccess m => Comment -> Bool -> m ()
voteComment c up = do
  let update | up        = $(getLabel 'cUpVotes)
             | otherwise = $(getLabel 'cDownVotes)
  c' <- runCommand
        [ "findAndModify" =: postColl
        , "query"         =: [$(getField 'cId) c]
        , "new"           =: True
        , "update"        =: ["$inc" =: [update =: (1 :: Int)]]
        ] >>= lookup "value" >>= fromBson
  let score = scoreComment c'
  runCommand [ "findAndModify" =: postColl
             , "query"         =: [ $(getField 'cId) c'
                                  , $(getField 'cUpVotes) c'
                                  , $(getField 'cDownVotes) c'
                                  ]
             , "update"        =: ["$set" =: [$(getLabel 'cScore) := (Float score)]]
             ]
  return ()

votePost :: (DbAccess m, MonadContext m) => (Either Submission Comment) -> Bool -> m ()
votePost (Left s) _   = voteSubmission s
votePost (Right c) up = voteComment c up
