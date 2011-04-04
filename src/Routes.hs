{-# Language DeriveDataTypeable #-}

module Routes where

import Control.Monad

import Data.Data               (Data, Typeable)
import qualified Data.Text as T

import Web.Routes
import Web.Routes.Happstack

import DB




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
           deriving (Read, Show, Eq, Ord, Typeable, Data)

instance PathInfo Route where
  toPathSegments (R_Listing list psort) = ["listing", show list, show psort]
  toPathSegments (R_Post id')           = ["post", show id']
  toPathSegments (R_Vote id' up)        = ["vote", show id', show up]
  toPathSegments  R_Submit              = ["submit"]
  toPathSegments (R_Comment id')        = ["comment", show id']
  toPathSegments (R_User username)      = ["user", T.unpack username]
    
  fromPathSegments =
    msum [ do segment "listing"
              list <- fmap read anySegment
              psort <- fmap read anySegment
              return $ R_Listing list psort
         , do segment "post"
              liftM R_Post $ fmap read anySegment
         , do segment "vote"
              id' <- fmap read anySegment
              up <- fmap read anySegment
              return $ R_Vote id' up
         , segment "submit" >> return R_Submit
         , do segment "comment"
              liftM R_Comment $ fmap read anySegment
         , do segment "user"
              liftM (R_User . T.pack) $ fmap read anySegment
         ]