{-# Language DeriveDataTypeable, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Types.Route
       ( -- * Types for the routing
         Route (..)
       , PageNumber
       , home
         
         -- * Redirecting
       , routeRedirect
       , redirectPage
       , redirectPageReferer
       ) where


import Data.Data                     (Data, Typeable)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Text.ParserCombinators.Parsec (optionMaybe)

import Control.Applicative           ((<*>), (<$>))
import Control.Monad

import Web.Routes
import Web.Routes.Happstack

import Happstack.Server

import Types.User
import Types.Post



type PageNumber = Int


data Route = R_Submissions Submissions PageNumber (Maybe UserName) PostSort
           | R_Comments PageNumber UserName PostSort
           | R_Post PostId PostSort
           | R_Vote PostId Bool
           | R_Delete PostId
           | R_Submit
           | R_User UserName
           | R_Login
           | R_Logout
           | R_Register
           | R_CP
           | R_Static [String]
           deriving (Read, Show, Eq, Ord, Typeable, Data)

home :: Route
home = R_Submissions Submissions 0 Nothing Top


instance PathInfo Route where
  toPathSegments (R_Submissions submissions page userM psort) =
    case userM of
      Nothing -> ["submissions", show submissions, show page, show psort]
      Just user -> ["submissions", show submissions, show page, show psort, user]
  toPathSegments (R_Comments page user psort) =
    ["comments", show page, user, show psort]
  toPathSegments (R_Post id' psort) = ["post", show id', show psort]
  toPathSegments (R_Vote id' up)    = ["vote", show id', show up]
  toPathSegments (R_Delete id')     = ["delete", show id']
  toPathSegments  R_Submit          = ["submit"]
  toPathSegments (R_User username)  = ["user", username]
  toPathSegments  R_Login           = ["login"]
  toPathSegments  R_Logout          = ["logout"]
  toPathSegments  R_Register        = ["register"]
  toPathSegments  R_CP              = ["cp"]
  toPathSegments (R_Static segs)    = "static" : segs
    
  -- Note that the "static" is left out on purpose, since we sould
  -- serve static files with fileServe before trying to dispatch the
  -- url.
  fromPathSegments =
    msum [ do segment "submissions"
              submissions <- readSegment
              page <- readSegment
              psort <- readSegment
              user <- optionMaybe anySegment
              return $ R_Submissions submissions page user psort
         , do segment "comments"
              R_Comments <$> readSegment <*> readSegment <*> readSegment
         , do segment "post"
              R_Post <$> readSegment <*> readSegment
         , do segment "vote"
              R_Vote <$> readSegment <*> readSegment
         , do segment "delete"
              R_Delete <$> readSegment
         , segment "submit" >> return R_Submit
         , do segment "user"
              R_User <$> anySegment
         , do segment "login"
              return R_Login
         , do segment "logout"
              return R_Logout
         , do segment "register"
              return R_Register
         , do segment "cp"
              return R_CP
         ]


readSegment :: Read a => URLParser a
readSegment = anySegment >>= readM
  where readM s | length res > 0 = return $ (fst . head) res
                | otherwise      = fail "Could not parse."
          where res = reads s


redirQuery :: String
redirQuery = "redir"


{-|

Appends the "redir" query to some 'Route'.

This should be used when we have to "remember" a redirect, for example
when using a form (e.g. the login form).

-}
routeRedirect :: (ServerMonad m, ShowURL m, URL m ~ Route) => URL m -> m Link
routeRedirect r = do
  uri <- liftM rqUri askRq
  showURLParams r [(redirQuery, uri)]



{-|

Redirects the page, with this criteria:

If the query "redir" is present, redirect there

Otherwise, go to the home.

-}
redirectPage ::
  ( ServerMonad m
  , FilterMonad Response m
  , ShowURL m
  , URL m ~ Route
  ) => m Response
redirectPage = do
  qRedir <- liftM rqInputsQuery askRq >>= \query -> return $ do
    i <- lookup redirQuery query
    either (const Nothing) Just (inputValue i)
  case qRedir of
    Just redir -> liftM toResponse (seeOther (BL.unpack redir) "")
    Nothing -> seeOtherURL home


-- | Same as above, but also looks for a referer.
redirectPageReferer ::
  ( ServerMonad m
  , FilterMonad Response m
  , ShowURL m
  , URL m ~ Route
  ) => m Response
redirectPageReferer = do
  qRedir <- liftM rqInputsQuery askRq >>= \query -> return $ do
    i <- lookup redirQuery query
    either (const Nothing) Just (inputValue i)
  rRedir <- liftM (getHeader "Referer") askRq
  case qRedir of
    Just redir -> seeOther' $ BL.unpack redir
    Nothing -> case rRedir of
      Just redir -> seeOther' $ B.unpack redir
      Nothing -> seeOtherURL home
  where
    seeOther' uri = liftM toResponse (seeOther uri "")