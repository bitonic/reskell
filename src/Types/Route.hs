{-# Language DeriveDataTypeable, TypeFamilies #-}

module Types.Route (
    PostListing (..)
  , PostSort (..)
  , Route (..)
  , home
  , routeRedirect
  , getRedirect
  ) where


import Data.Data                     (Data, Typeable)
import Data.Maybe                    (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B8

import Control.Monad

import Web.Routes

import Happstack.Server

import Types.User
import Types.Post



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
           | R_Login
           | R_Logout
           | R_Register
           | R_Static [String]
           deriving (Read, Show, Eq, Ord, Typeable, Data)

home :: Route
home = R_Listing Submissions Top

instance PathInfo Route where
  toPathSegments (R_Listing list psort) = ["listing", show list, show psort]
  toPathSegments (R_Post id')           = ["post", show id']
  toPathSegments (R_Vote id' up)        = ["vote", show id', show up]
  toPathSegments  R_Submit              = ["submit"]
  toPathSegments (R_Comment id')        = ["comment", show id']
  toPathSegments (R_User username)      = ["user", username]
  toPathSegments  R_Login               = ["login"]
  toPathSegments  R_Logout              = ["logout"]
  toPathSegments  R_Register            = ["register"]
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
              return $ R_Vote id' up
         , segment "submit" >> return R_Submit
         , do segment "comment"
              liftM R_Comment (anySegment >>= readM)
         , do segment "user"
              liftM R_User anySegment
         , do segment "login"
              return R_Login
         , do segment "logout"
              return R_Logout
         , do segment "register"
              return R_Register
         ]
    
  

readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse."
  where
    res = reads s


redirQuery :: String
redirQuery = "redir"
             
routeRedirect :: (ServerMonad m, ShowURL m, URL m ~ Route) => URL m -> m Link
routeRedirect r = do
  uri <- liftM rqUri askRq
  showURLParams r [(redirQuery, uri)]



getRedirect :: (ServerMonad m, ShowURL m, URL m ~ Route) => m Link
getRedirect = do
  query <- liftM rqInputsQuery askRq
  homeURL <- showURL home
  return $ fromMaybe homeURL $ do
    i <- lookup redirQuery query
    liftM B8.unpack $ either (const Nothing) Just (inputValue i)