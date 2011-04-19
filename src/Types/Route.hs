{-# Language DeriveDataTypeable #-}

module Types.Route (
    PostListing (..)
  , PostSort (..)
  , Route (..)
  , home
  ) where


import Data.Data                     (Data, Typeable)

import Control.Monad

import Text.ParserCombinators.Parsec (option)

import Web.Routes

import Types.User
import Types.Post

data PostListing = Asks | Links | Submissions | Comments
                 deriving (Read, Show, Eq, Ord, Typeable, Data)

data PostSort = New | Top
              deriving (Read, Show, Eq, Ord, Typeable, Data)

data Route = R_Listing PostListing PostSort
           | R_Post PostId
           | R_Vote PostId Bool Route
           | R_Submit
           | R_Comment PostId
           | R_User UserName
           | R_Login Route
           | R_Logout Route
           | R_Register Route
           | R_Static [String]
           deriving (Read, Show, Eq, Ord, Typeable, Data)

home :: Route
home = R_Listing Submissions Top

instance PathInfo Route where
  toPathSegments (R_Listing list psort) = ["listing", show list, show psort]
  toPathSegments (R_Post id')           = ["post", show id']
  toPathSegments (R_Vote id' up route)  = "vote" : show id' : show up :
                                          toPathSegments route
  toPathSegments  R_Submit              = ["submit"]
  toPathSegments (R_Comment id')        = ["comment", show id']
  toPathSegments (R_User username)      = ["user", username]
  toPathSegments (R_Login route)        = "login" : toPathSegments route
  toPathSegments (R_Logout route)       = "logout" : toPathSegments route
  toPathSegments (R_Register route)     = "register" : toPathSegments route
  toPathSegments (R_Static segs)        = "static" : segs
    
  -- Note that the "static" is left out on purpose, since we sould
  -- serve static files with fileServe before trying to dispatch the
  -- url.
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
              route <- option home fromPathSegments
              return $ R_Vote id' up route
         , segment "submit" >> return R_Submit
         , do segment "comment"
              liftM R_Comment (anySegment >>= readM)
         , do segment "user"
              liftM R_User anySegment
         , do segment "login"
              route <- option home fromPathSegments
              return $ R_Login route
         , do segment "logout"
              route <- option home fromPathSegments
              return $ R_Logout route
         , do segment "register"
              route <- option home fromPathSegments
              return $ R_Register route
         ]
    
  

readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse."
  where
    res = reads s
