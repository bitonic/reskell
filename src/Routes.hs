{-# Language DeriveDataTypeable #-}

module Routes (
    PostListing (..)
  , PostSort (..) 
  , Route
  , runRoutes
  ) where

import Control.Monad

import Data.Data               (Data, Typeable)
import qualified Data.Text as T

import Happstack.Server

import Web.Routes
import Web.Routes.Happstack

import DB
import Context
import Pages



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
         ]

readM :: (Read a, Monad m) => String -> m a
readM s | length res > 0 = return $ (fst . head) res
        | otherwise      = fail "Could not parse url."
  where
    res = reads s

siteSpec = setDefault (R_Listing Links New) $
           Site { handleSite = \f u -> unRouteT (dispatch u) f
                , formatPathSegments = \u -> (toPathSegments u, [])
                , parsePathSegments  = parseSegments fromPathSegments
                }

dispatch :: Route -> RouteT Route ContextM Response
dispatch (R_Post id') = ok $ toResponse ("This is post n " ++ (show id') ++ "!")
dispatch u            = ok $ toResponse $ show u

runRoutes :: Context -> ServerPart Response
runRoutes context = mapServerPartT (unpackContext context) $ implSite "/" "" siteSpec