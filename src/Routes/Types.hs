{-# Language DeriveDataTypeable #-}

module Routes.Types (
    PostListing (..)
  , PostSort (..)
  , Route (..)
  ) where


import Control.Monad

import Data.Data               (Data, Typeable)
import qualified Data.Text as T
import Data.Maybe              (fromMaybe)

import Text.ParserCombinators.Parsec (option)

import Web.Routes

import Types

data PostListing = Asks | Links | Submissions | Comments
                 deriving (Read, Show, Eq, Ord, Typeable, Data)

data PostSort = New | Top
              deriving (Read, Show, Eq, Ord, Typeable, Data)

data Route = R_Listing PostListing PostSort
           | R_Post PostId
           | R_Vote PostId Bool
           | R_Submit
           | R_Comment PostId
           | R_User UserName
           | R_Login Route
           deriving (Read, Show, Eq, Ord, Typeable, Data)

home :: Route
home = R_Listing Submissions Top

instance PathInfo Route where
  toPathSegments (R_Listing list psort) = ["listing", show list, show psort]
  toPathSegments (R_Post id')           = ["post", show id']
  toPathSegments (R_Vote id' up)        = ["vote", show id', show up]
  toPathSegments  R_Submit              = ["submit"]
  toPathSegments (R_Comment id')        = ["comment", show id']
  toPathSegments (R_User username)      = ["user", T.unpack username]
  toPathSegments (R_Login route)        = "login" : toPathSegments route
    
  fromPathSegments =
    msum [ do segment "listing"
              list <- anySegment >>= readM
              psort <- anySegment >>= readM
              return $ R_Listing list psort
         , do segment "post"
              liftM R_Post (anySegment >>= readM)
         , do segment "vote"
              id' <- anySegment >>= readM
              up <- anySegment >>= readM
              return $ R_Vote id' up
         , segment "submit" >> return R_Submit
         , do segment "comment"
              liftM R_Comment (anySegment >>= readM)
         , do segment "user"
              liftM (R_User . T.pack) (anySegment >>= readM)
         , do segment "login"
              route <- option home fromPathSegments
              return $ R_Login route
         ]
  
readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse."
  where
    res = reads s