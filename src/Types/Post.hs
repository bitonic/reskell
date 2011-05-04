{-# Language TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Post
       ( -- * Types
         PostId
       , Post (..)       
       , SContent (..)
       , Submission (..)       
       , Comment (..)

         -- * Indexes
       , ScoreIx (..)
       , TimeIx (..)
       , IdIx (..)
       , ParentIx (..)
         
         -- * Sorting and selecting
       , PostSort (..)
       , Submissions (..)

         -- * DB
       , PostDB (..)
       , PostVoters
       , openPostDB
         
         -- * Query / Updates
       , NewSubmission (..)
       , NewComment (..)
       , GetPost (..)
       , GetSubmission (..)
       , GetSubmissions (..)
       , GetComments (..)
       , VoteSubmission (..)
       , VoteComment (..)
       , VotePost (..)
       , CountComments (..)
       , DeletePost (..)
         
         -- * Utils
       , getDomain
       ) where

import Control.Monad.Reader
import Control.Monad.State     (modify, get)
import Control.Monad.Identity  (Identity)

import System.FilePath         ((</>))

import Data.Data               (Data, Typeable)
import Data.Time.Clock
import Data.SafeCopy
import Data.Set                (Set)
import qualified Data.Set as S
import Data.Acid


import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator


import Web.Routes              ()

import Happstack.Data.IxSet    (IxSet, (@=), Indexable, ixFun, ixSet)
import qualified Happstack.Data.IxSet as Ix
import Happstack.Data.Proxy

import Types.Common            ()
import Types.User



type PostId = Int
-- | Type to store the users who voted in a Post
type PostVoters = Set UserName

-- | Post type, mainly used to generalize functions that render stuff
-- in Pages/, since 'Submission' and 'Comment' are really similar.
class Post a where
  pId        :: a -> PostId
  pTime      :: a -> UTCTime
  pUserName  :: a -> UserName
  pVotesDown :: a -> PostVoters
  pVotesUp   :: a -> PostVoters



-- | Actual content of a 'Submission'.
data SContent = Ask String | Link String String
              deriving (Eq, Ord, Show, Read, Data, Typeable)


data Submission = Submission { sId        :: PostId
                             , sUserName  :: UserName
                             , sTime      :: UTCTime
                             , sTitle     :: String
                             , sContent   :: SContent
                             , sVotesUp   :: PostVoters
                             , sVotesDown :: PostVoters
                             , sScore     :: Double
                             }
                deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Post Submission where
  pId        = sId
  pTime      = sTime
  pUserName  = sUserName
  pVotesUp   = sVotesUp
  pVotesDown = sVotesDown

data Comment = Comment { cId         :: PostId
                       , cUserName   :: String
                       , cTime       :: UTCTime
                       , cText       :: String
                       , cVotesUp    :: PostVoters
                       , cVotesDown  :: PostVoters
                       , cParent     :: PostId
                       , cSubmission :: PostId
                       , cScore      :: Double
                       }
             deriving (Eq, Ord, Show, Read, Data, Typeable)
  
instance Post Comment where
  pId        = cId
  pTime      = cTime
  pUserName  = cUserName
  pVotesUp   = cVotesUp
  pVotesDown = cVotesDown
  
  

-- | Type to select 'Submission' based on their type, used in two
-- context: in the 'Route' type, to select what to display, and as an
-- index in the 'IxSet'.
data Submissions = Asks | Links | Submissions
                 deriving (Read, Show, Eq, Ord, Typeable, Data)

-- | To decide the ordering in the 'Route'.
data PostSort = New | Top
              deriving (Read, Show, Eq, Ord, Typeable, Data)


-- Various indexes to use in the 'IxSet'.
newtype ScoreIx = ScoreIx Double
                deriving (Eq, Ord, Data, Typeable)
newtype TimeIx = TimeIx UTCTime
               deriving (Eq, Ord, Data, Typeable)
newtype IdIx = IdIx PostId
             deriving (Eq, Ord, Data, Typeable)
newtype ParentIx = ParentIx PostId
                 deriving (Eq, Ord, Data, Typeable)
newtype SubmissionIx = SubmissionIx PostId
                     deriving (Eq, Ord, Data, Typeable)

instance Indexable Submission where
  empty = ixSet [ ixFun (\s -> [TimeIx     (sTime s)])
                , ixFun (\s -> [UserNameIx (sUserName s)])
                , ixFun (\s -> [ScoreIx    (sScore s)])
                , ixFun (\s -> [IdIx       (sId s)])
                , ixFun (\s -> case sContent s of
                            Link _ _ -> [Links]
                            Ask _    -> [Asks])
                ]

instance Indexable Comment where
  empty = ixSet [ ixFun (\c -> [IdIx         (cId c)])
                , ixFun (\c -> [TimeIx       (cTime c)])
                , ixFun (\c -> [UserNameIx   (cUserName c)])
                , ixFun (\c -> [ScoreIx      (cScore c)])
                , ixFun (\c -> [ParentIx     (cParent c)])
                , ixFun (\c -> [SubmissionIx (cSubmission c)])
                ]

  
-------------------------------------------------------------------------------
  
-- | The 'AcidState' that holds the 'Post's.
data PostDB = PostDB { submissionSet :: IxSet Submission
                       -- ^ The set of 'Submission's
                     , commentSet    :: IxSet Comment
                       -- ^ The set of comments
                     , scoringStart  :: UTCTime
                       -- ^ This stores the time in which the scoring
                       -- of the 'Post's has started, used in the
                       -- scoring functions.
                     , postIdCounter :: PostId
                       -- ^ A counter to assign a unique 'PostId' to
                       -- the posts.
                     }

$(deriveSafeCopy 0 'base ''SContent)
$(deriveSafeCopy 0 'base ''Submission)
$(deriveSafeCopy 0 'base ''Comment)
$(deriveSafeCopy 0 'base ''PostDB)
$(deriveSafeCopy 0 'base ''Submissions)
$(deriveSafeCopy 0 'base ''PostSort)

-- | This initializes the 'PostDB', if it hasn't initialized
-- already. This function has to be called when the application
-- starts.
openPostDB :: FilePath -> IO (AcidState PostDB)  
openPostDB fp = do
  now <- getCurrentTime
  openAcidStateFrom (fp </> "PostDB") $ PostDB Ix.empty Ix.empty now 0
  
-----------------------------------------------------------------------

-- | Increments the post counter atomically.
incPostCounter :: Update PostDB PostId
incPostCounter = do                  
  id' <- liftM ((+1) . postIdCounter) get
  modify (\s -> s {postIdCounter = id'})
  return id'


newSubmission :: UserName -> String -> SContent -> UTCTime -> Update PostDB Submission
newSubmission userName title content now = do
  id' <- incPostCounter
  let submission = Submission { sId = id'
                              , sUserName  = userName
                              , sTime      = now
                              , sTitle     = title
                              , sContent   = content
                              -- We add the user that's posting to the
                              -- up voters automatically
                              , sVotesUp   = S.fromList [userName]
                              , sVotesDown = S.empty
                              , sScore     = 0
                              }
  score <- runQuery $ scoreSubmission submission
  let submission' = submission {sScore = score}
  modify (\s -> s {submissionSet = Ix.insert submission' (submissionSet s)})
  return submission'

newComment :: UserName -> String -> PostId -> PostId -> UTCTime -> Update PostDB Comment
newComment userName text sId' parentId now = do
  id' <- incPostCounter
  let comment = Comment { cId         = id'
                        , cTime       = now
                        , cUserName   = userName
                        , cText       = text
                        , cVotesUp    = S.fromList [userName]
                        , cVotesDown  = S.empty
                        , cParent     = parentId
                        , cSubmission = sId'
                        , cScore      = 0
                        }
      comment' = comment {cScore = scoreComment comment}
  modify (\s -> s {commentSet = Ix.insert comment' (commentSet s)})
  return comment'

-- | Scores a post. Taken straight from the reddit algorithm.
scoreSubmission :: Submission -> Query PostDB Double
scoreSubmission submission = do
  start <- asks scoringStart
  let fi      = fromInteger . toInteger
      s       = S.size (sVotesUp submission) - S.size (sVotesDown submission)
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
    pos  = realToFrac (S.size (cVotesUp comment))
    n    = pos + realToFrac (S.size $ cVotesDown comment)
    phat = pos / n
    z    = 1.644853646608357 -- 95% confidence, Statistics2.pnormaldist(1-0.2/2)


getSubmission :: PostId -> Query PostDB (Maybe Submission)
getSubmission id' = liftM (Ix.getOne . (@= IdIx id')) $ asks submissionSet

getPost :: PostId -> Query PostDB (Maybe (Either Submission Comment))
getPost id' = do
  db <- ask
  return $ case Ix.getOne (submissionSet db @= IdIx id') of
    Nothing -> liftM Right (Ix.getOne (commentSet db @= IdIx id'))
    Just s  -> Just $ Left s



userSel   :: (Ix.Indexable a, Typeable a, Ord a) => Maybe UserName -> IxSet a -> IxSet a
parentSel :: (Ix.Indexable a, Typeable a, Ord a) => Maybe PostId -> IxSet a -> IxSet a
userSel   = maybe id (\user -> (@= UserNameIx user))
parentSel = maybe id (\parentId -> (@= ParentIx parentId))

sortIx :: (Ix.Indexable a, Typeable a) => PostSort -> IxSet a -> [a]
sortIx New = Ix.toDescList (Proxy :: Proxy TimeIx)
sortIx Top = Ix.toDescList (Proxy :: Proxy ScoreIx)

-- | Gets a list of submissions
getSubmissions :: Submissions
                  -> PostSort -- ^ The sort to use
                  -> Int      -- ^ The number of submissions to return
                  -> Int      -- ^ The number of submissions to skip
                  -> Maybe UserName
                  -- ^ If provided, only selects the submission by the user.
                  -> Query PostDB [Submission]
getSubmissions listing psort l s userM = do
  submissions <- asks submissionSet
  return (limit . drop s . sortIx psort . stype . userSel userM $ submissions)
  where
    limit | l < 0     = id
          | otherwise = take l
    stype = case listing of
      Submissions -> id
      Asks -> (@= Asks)
      Links -> (@= Links)

-- | Gets a list of 'Comment's.
getComments :: PostSort
               -> Maybe PostId
               -- ^ If provided, only gets the comments to the 'PostId' post
               -> Maybe UserName
               -- ^ If provided, only gets the comment by the user
               -> Query PostDB [Comment]
getComments psort parentIdM userM = do
  comments <- asks commentSet
  return (sortIx psort . parentSel parentIdM . userSel userM $ comments)
  

-- | Takes a post, a user, a bool to indicate whether the vote is up
-- or down, and a list of functions that operate on the post to modify
-- it accordingly, returns the voted post.
-- If the user is voting up and has already voted up, the vote will be
-- removed. If the user is voting up and has voted down the down vote
-- will be removed, and the up vote will be added. The opposite
-- applies for down votes.
votePost' :: Post a
             => a
             -> User
             -> Bool
             -> (a -> a) -- ^ The function that removes the up vote
             -> (a -> a) -- ^ The function the adds the up vote
             -> (a -> a) -- ^ The function that removes the down vote
             -> (a -> a) -- ^ The function that adds the down vote
             -> a
votePost' post user up unVoteUp voteUp unVoteDown voteDown = f post
  where
    userName  = uName user
    votesUp   = pVotesUp post
    votesDown = pVotesDown post
    votedUp   = S.member userName votesUp
    votedDown = S.member userName votesDown
    f | up && votedUp   = unVoteUp
      | up && votedDown = unVoteDown . voteUp
      | up             = voteUp
      | votedDown      = unVoteDown
      | votedUp        = unVoteUp . voteDown
      | otherwise      = voteDown

voteSubmission :: Submission -> Bool -> User -> Update PostDB ()
voteSubmission submission up user = do
  let unVoteUp    = \s -> s {sVotesUp   = S.delete (uName user) (sVotesUp s)}
      voteUp      = \s -> s {sVotesUp   = S.insert (uName user) (sVotesUp s)}
      unVoteDown  = \s -> s {sVotesDown = S.delete (uName user) (sVotesDown s)}
      voteDown    = \s -> s {sVotesDown = S.insert (uName user) (sVotesDown s)}
      -- Vote the submission
      submission' = votePost' submission user up unVoteUp voteUp unVoteDown voteDown
  -- Get the new score
  score <- runQuery $ scoreSubmission submission'
  let submission'' = submission' {sScore = score}
  -- Modify the set
  modify (\s -> s {submissionSet = Ix.updateIx (IdIx (sId submission)) submission'' (submissionSet s)})

voteComment :: Comment -> Bool -> User -> Update PostDB ()
voteComment comment up user = do
  let unVoteUp    = \c -> c {cVotesUp   = S.delete (uName user) (cVotesUp c)}
      voteUp      = \c -> c {cVotesUp   = S.insert (uName user) (cVotesUp c)}
      unVoteDown  = \c -> c {cVotesDown = S.delete (uName user) (cVotesDown c)}
      voteDown    = \c -> c {cVotesDown = S.insert (uName user) (cVotesDown c)}
      comment'    = votePost' comment user up unVoteUp voteUp unVoteDown voteDown
      comment''   = comment' {cScore = scoreComment comment'}
  modify (\s -> s {commentSet = Ix.updateIx (IdIx (cId comment)) comment'' (commentSet s)})
  

-- | Gets a 'PostId', sees if it refers to a 'Submission' or to a
-- 'Comment', and votes it. Does nothing if the post is not present.
votePost :: (Either Submission Comment) -> Bool -> User -> Update PostDB ()
votePost post up user =
  either (\s -> voteSubmission s up user) (\c -> voteComment c up user) post

-- | Returns the number of votes to a post.
-- TODO: right now it just gets the top level votes, I need to find a
-- good way to get all the votes.
countComments :: PostId -> Query PostDB Int
countComments pId' = liftM (Ix.size . (@= SubmissionIx pId')) $ asks commentSet

deletePost :: PostId -> Update PostDB ()
deletePost id' = do
  comments <- runQuery $ getComments New (Just id') Nothing
  mapM_ (deletePost . cId) comments
  postM <- runQuery $ getPost id'
  case postM of
    Nothing -> return ()
    Just (Left sub) -> modify (\s -> s {submissionSet = Ix.delete sub (submissionSet s)})
    Just (Right comm) -> modify (\s -> s {commentSet = Ix.delete comm (commentSet s)})
  

$(makeAcidic ''PostDB [ 'newSubmission
                      , 'newComment
                      , 'getPost
                      , 'getSubmission
                      , 'getSubmissions
                      , 'getComments
                      , 'voteSubmission
                      , 'voteComment
                      , 'votePost
                      , 'countComments
                      , 'deletePost
                      ])  
  


-------------------------------------------------------------------------------

parseProtocol :: ParsecT String u Identity String
parseProtocol = string "http://" <|> string "https://"

parseDomain :: ParsecT String u Identity String
parseDomain = do
  parseProtocol
  d1 <- liftM ('.' :) domainLetters
  d2 <- domainSeg
  rest <- manyTill domainSeg $ endDomain
                          <|> (char ':' >> many1 digit >> endDomain)
  let all' = d1 : d2 : rest
  return $ tail $ concat $ drop (length all' - 2) all'
  where
    domainSeg = do
      char '.'
      d <- domainLetters
      return $ '.' : d
    domainLetters = many1 $ oneOf $ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    endDomain = eof <|> (char '/' >> return ())

-- | Utility function that parses an url, and returns the domain.
getDomain :: String -> Maybe String
getDomain url = case parse parseDomain "" url of
  Right p -> Just p
  Left _  -> Nothing
