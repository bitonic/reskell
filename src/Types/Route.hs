{-# Language DeriveDataTypeable, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Types.Route (
    Submissions (..)
  , PostSort (..)
  , Route (..)
  , PageNumber
  , home
  , routeRedirect
  , redirectPage
  , redirectPageReferer
  ) where


import Data.Data                     (Data, Typeable)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Word                     (Word32)

import Control.Monad

import Web.Routes
import Web.Routes.Happstack

import Happstack.Server

import Types.User
import Types.Post


data Submissions = Asks | Links | Submissions
                 deriving (Read, Show, Eq, Ord, Typeable, Data)

data PostSort = New | Top
              deriving (Read, Show, Eq, Ord, Typeable, Data)

type PageNumber = Word32

data Route = R_Submissions Submissions PostSort PageNumber
           | R_Comments PostSort PageNumber
           | R_Post PostId
           | R_Vote PostId Bool
           | R_Submit
           | R_Comment PostId
           | R_User UserName
           | R_Login
           | R_Logout
           | R_Register
           | R_CP
           | R_Static [String]
           deriving (Read, Show, Eq, Ord, Typeable, Data)

home :: Route
home = R_Submissions Submissions Top 0


instance PathInfo Route where
  toPathSegments (R_Submissions submissions psort page) =
    ["submissions", show submissions, show psort, show page]
  toPathSegments (R_Comments psort page) =
    ["comments", show psort, show page]
  toPathSegments (R_Post id')      = ["post", show id']
  toPathSegments (R_Vote id' up)   = ["vote", show id', show up]
  toPathSegments  R_Submit         = ["submit"]
  toPathSegments (R_Comment id')   = ["comment", show id']
  toPathSegments (R_User username) = ["user", username]
  toPathSegments  R_Login          = ["login"]
  toPathSegments  R_Logout         = ["logout"]
  toPathSegments  R_Register       = ["register"]
  toPathSegments  R_CP             = ["cp"]
  toPathSegments (R_Static segs)   = "static" : segs
    
  -- Note that the "static" is left out on purpose, since we sould
  -- serve static files with fileServe before trying to dispatch the
  -- url.
  fromPathSegments =
    msum [ do segment "submissions"
              list <- readSegment
              psort <- readSegment
              page <- readSegment
              return $ R_Submissions list psort page
         , do segment "comments"
              psort <- readSegment
              page <- readSegment
              return $ R_Comments psort page
         , do segment "post"
              liftM R_Post readSegment
         , do segment "vote"
              id' <- readSegment
              up <- readSegment
              return $ R_Vote id' up
         , segment "submit" >> return R_Submit
         , do segment "copmment"
              liftM R_Comment readSegment
         , do segment "user"
              liftM R_User anySegment
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


-- | Appends the "redir" query to some Route. This should be used when
-- we have to "remember" a redirect, for example when using a form
-- (e.g. the login form).
routeRedirect :: (ServerMonad m, ShowURL m, URL m ~ Route) => URL m -> m Link
routeRedirect r = do
  uri <- liftM rqUri askRq
  showURLParams r [(redirQuery, uri)]



-- | Redirects the page, with this criteria:
-- If the query "redir" is present, redirect there
-- Otherwise, go to the home.
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