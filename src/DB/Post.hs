{-# Language OverloadedStrings, DeriveDataTypeable, TemplateHaskell, 
    ScopedTypeVariables #-}

module DB.Post (
    initScoring
  , newSubmission
  , newComment
  , getSubmission
  , getComment
  , getPost
  , getSubmissions
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
  

-- | Increments the post counter atomically.
incPostCounter :: DbAccess m => m PostId
incPostCounter =
  findAndModify FindAndModify { famSelector = select ["_id" =: postCounter] postColl
                              , famSort     = []
                              , famRemove   = False
                              , famUpdate   = ["$inc" =: ["counter" =: (1 :: PostId)]]
                              , famNew      = True
                              , famUpsert   = True
                              } >>= lookup "counter"


newSubmission :: (MonadIO m, DbAccess m, MonadContext m)
                 => UserName -> String -> SContent -> m Submission
newSubmission username title content = do
  time <- liftIO getCurrentTime
  id' <- incPostCounter
  let submission = Submission { sId        = id'
                              , sUserName  = username
                              , sTime      = time
                              , sTitle     = title
                              , sContent   = content
                              , sVotesUp   = 0
                              , sVotesDown = 0
                              , sScore     = 0
                              , sVoters    = []
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
                        , cVotesUp    = 0
                        , cVotesDown  = 0
                        , cParent     = pId parent
                        , cSubmission = submissionid
                        , cScore      = 0
                        , cVoters     = []
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

                  
getSubmissions :: DbAccess m
                  => Submissions -> PostSort -> Limit -> Word32 -> m [Submission]
getSubmissions listing psort l s =
  getPosts (select (selectField listing) postColl)
    { limit = l
    , skip  = s
    , sort  = [sortField psort, $(getLabel 'sTime) =: (-1 :: Int)]
    }
  where
    sortField New = $(getLabel 'sTime) =: (-1 :: Int)
    sortField Top = $(getLabel 'sScore) =: (-1 :: Int)
    
    selectField Asks = subDocument $(getLabel 'sContent) $(getConsDoc 'Ask)
    selectField Links = subDocument $(getLabel 'sContent) $(getConsDoc 'Link)
    selectField Submissions = $(getConsDoc 'Submission)


getComments :: (DbAccess m, Post a) => a -> m [Comment]
getComments p = getPosts (select [$(getLabel 'cParent) =: pId p] postColl)
                  {sort = [ $(getLabel 'cScore) =: (-1 :: Int)
                          , $(getLabel 'cTime) =: (-1 :: Int)]}

countComments :: (DbAccess m, Post a) => a -> m Int
countComments p = count (select [$(getLabel 'cParent) =: pId p] postColl)


-- | Scores a post. Taken straight from the reddit algorithm.
scoreSubmission :: (MonadContext m, DbAccess m) => Submission -> m Double
scoreSubmission submission = do
  start <- askContext scoringStart
  let fi      = fromInteger . toInteger
      s       = sVotesUp submission - sVotesDown submission
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
  else phat+z*z/(2*n)-z* sqrt ((phat*(1-phat)+z*z/(4*n))/n) / (1+z*z/n)
  where
    pos  = realToFrac $ cVotesUp comment
    n    = pos + realToFrac (cVotesDown comment)
    phat = pos / n
    z    = 1.644853646608357 -- 95% confidence, Statistics2.pnormaldist(1-0.2/2)
    

voteSubmission :: (MonadContext m, DbAccess m)
                  => Submission -> Bool -> User -> m ()
voteSubmission s up user = do
  let update | up        = $(getLabel 'sVotesUp)
             | otherwise = $(getLabel 'sVotesDown)
  -- This is quite tricky. This query selects the submission with the
  -- given id, and checking that the user that's voting hasn't voted
  -- before, while increasing the up/down votes and adding the user to
  -- the list of voters.
  s' <- findAndModify FindAndModify
         { famSelector = select [ $(getField 'sId) s
                                , $(getLabel 'sVoters) =: ["$ne" =: uName user]
                                ] postColl
         , famSort     = []
         , famRemove   = False
         , famUpdate   = [ "$inc"  =: [update =: (1 :: Int)]
                         , "$addToSet" =: [$(getLabel 'sVoters) =: uName user]
                         ]
         , famNew      = True
         , famUpsert   = False
         } >>= fromBson
  score <- scoreSubmission s'
  -- Note that I select the post based on the id *and* the number of
  -- votes. In this way we are sure of updating the post only if the
  -- post is in the same state that we received it. In this way we
  -- avoid race conditions.
  findAndModify FindAndModify { famSelector = select [ $(getField 'sId) s'
                                                     , $(getField 'sVotesUp) s'
                                                     , $(getField 'sVotesDown) s'
                                                     ] postColl
                              , famSort     = []
                              , famRemove   = False
                              , famUpdate   = ["$set" =: [$(getLabel 'sScore) =: score]]
                              , famNew      = False
                              , famUpsert   = False
                              }
  return ()


voteComment :: DbAccess m => Comment -> Bool -> User -> m ()
voteComment c up user = do
  let update | up        = $(getLabel 'cVotesUp)
             | otherwise = $(getLabel 'cVotesDown)
  c' <- findAndModify FindAndModify
         { famSelector = select [ $(getField 'cId) c
                                , $(getLabel 'cVoters) =: ["$ne" =: uName user]
                                ] postColl
         , famSort     = []
         , famRemove   = False
         , famUpdate   = [ "$inc"  =: [update =: (1 :: Int)]
                         , "$addToSet" =: [$(getLabel 'cVoters) =: uName user]
                         ]
         , famNew      = True
         , famUpsert   = False
         } >>= fromBson                           
  let score = scoreComment c'
  findAndModify FindAndModify { famSelector = select [ $(getField 'cId) c'
                                                     , $(getField 'cVotesUp) c'
                                                     , $(getField 'cVotesDown) c'
                                                     ] postColl
                              , famSort     = []
                              , famRemove   = False
                              , famUpdate   = ["$set" =: [$(getLabel 'cScore) =: score]]
                              , famNew      = False
                              , famUpsert   = False
                              }
  return ()

votePost :: (DbAccess m, MonadContext m)
            => Either Submission Comment -> Bool -> User -> m ()
votePost (Left s)  up user = voteSubmission s up user
votePost (Right c) up user = voteComment c up user
